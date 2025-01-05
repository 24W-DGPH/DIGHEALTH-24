#load packages -------
pacman::p_load(
  rio,            #importing data
  here,           #relative file pathways
  janitor,        #data cleaning and tables
  epikit,         #age_categories()functions
  tidyverse,      #data management and visualization
  haven,          #importing SPSS files
  shiny,          #install shiny
  ggplot2,        #install
  dplyr,          #install
  DT,             #install  
  corrplot,       #install
  tm,             #install
  wordcloud,      #install
  plotly,         #install Darstellung interaktive Balken-/Tortendiagramme
  reshape2,       #install
  RColorBrewe,   #install
  BiocManager.    #install
)

# Benötigte Pakete laden
library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(corrplot)
library(tm)
library(wordcloud)
library(reshape2)
library(RColorBrewer)
library(BiocManager)

# Daten laden
#data <- read.csv("/Applications/Progredienzangst_SPSS_09.05.2019_final Kopie 2.sav")

# Beispiel-Daten (ersetze dies durch deinen Datensatz)
set.seed(123)
data <- data.frame(
  Geschlecht = sample(c("Männlich", "Weiblich"), 100, replace = TRUE),
  Alter = sample(30:80, 100, replace = TRUE),
  Krankheitsstadium = sample(1:5, 100, replace = TRUE),
  Depression = runif(100, 0, 10),
  Fear_of_Progression = sample(c(0, 1), 100, replace = TRUE, prob = c(0.7, 0.3)),
  Lebensqualität = runif(100, 0, 100),
  Krankheitsdauer = sample(1:20, 100, replace = TRUE),
  Text = replicate(100, paste(sample(letters, 10, replace = TRUE), collapse = ""))
)

# Manuell eingegebene Inzidenz von Parkinson in Deutschland
parkinson_inzidenz_deutschland <- 112  # pro 100.000 Einwohner

# UI-Definition
ui <- fluidPage(
  # Überschrift
  titlePanel(h1("Fear of Progression in Parkinson's Patients", align = "left")),
  
  # Statistische Felder
  fluidRow(
    column(width = 4,
           div(class = "well",
               h4("Fear of Progression"),
               p(paste0(
                 round(mean(data$Fear_of_Progression) * 100, 2),        #hier Daten aus dem Datensatz einfügen
                 "% "))
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Incidence of Parkinson's disease in Germany"),
               p(paste0(parkinson_inzidenz_deutschland, "per 100.000"))
           )
    ),
    column(
      width = 4,
      div(class = "well",
          h4("Average age at diagnosis"),
          p(paste0(round(mean(data$Alter), 1), " years"))
      )
    )
  ),
  
  # Navigationsleiste
  navbarPage(
    title = "Navigation",
    
    # Demographische Daten
    tabPanel(
      "Demographische Daten",
      sidebarLayout(
        sidebarPanel(
          selectInput("variable", "Wähle eine Variable:", 
                      choices = c("Geschlecht", "Alter", "Krankheitsstadium", "Krankheitsdauer"))
        ),
        mainPanel(
          fluidRow(
            # Für Alter wird ein Histogramm angezeigt, ansonsten Balken- und Tortendiagramm
            uiOutput("demographic_plots"),
            dataTableOutput("demographic_table")
          )
        )
      )
    ),
    
    # Korrelationen
    tabPanel(
      "Korrelationen",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "correlation_vars",
            "Wähle Variablen für die Heatmap:",
            choices = names(data)[sapply(data, is.numeric)],
            selected = names(data)[sapply(data, is.numeric)]
          )
        ),
        mainPanel(
          fluidRow(
            column(6, plotOutput("correlation_heatmap")),
            column(6, plotOutput("scatterplot_correlation"))
          )
        )
      )
    ),
    
    # Regression
    tabPanel(
      "Regression",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(
            "regression_vars", 
            "Wähle Variablen für die Regression:",
            choices = c("Alter", "Geschlecht", "Krankheitsstadium", "Depression", "Lebensqualität"),
            selected = c("Alter", "Geschlecht", "Krankheitsstadium", "Depression")
          )
        ),
        mainPanel(plotOutput("regression_plot"))
      )
    ),
    
    # Textanalyse
    tabPanel("Textanalyse", plotOutput("wordcloud"))
  )
)

# Server-Logik
server <- function(input, output, session) {
  
  # Demographische Daten: Balken-, Tortendiagramm oder Verteilungsdiagramm
  output$demographic_plots <- renderUI({
    if (input$variable == "Alter") {
      # Verteilungsdiagramm (Histogramm) für Alter
      plotOutput("distribution_plot")
    } else {
      # Balken- und Tortendiagramm
      fluidRow(
        column(6, plotlyOutput("bar_plot")),
        column(6, plotlyOutput("pie_chart"))
      )
    }
  })
  
  output$distribution_plot <- renderPlot({
    req(input$variable == "Alter")
    ggplot(data) +
      geom_histogram(aes(x = Alter), binwidth = 5, fill = "steelblue", alpha = 0.7) +
      geom_histogram(aes(x = Alter - Krankheitsdauer), binwidth = 5, fill = "orange", alpha = 0.5) +
      labs(
        title = "Verteilung des Alters und Alter bei Diagnose",
        x = "Alter",
        y = "Häufigkeit"
      ) +
      theme_minimal()
  })
  
  output$bar_plot <- renderPlotly({
    req(input$variable)
    var_data <- data[[input$variable]]
    
    if (is.factor(var_data) || is.character(var_data)) {
      plot_data <- as.data.frame(table(var_data))
      colnames(plot_data) <- c("Category", "Count")
      
      plot_ly(
        data = plot_data,
        x = ~Category,
        y = ~Count,
        type = "bar",
        name = "Balkendiagramm",
        marker = list(color = "steelblue")
      ) %>%
        layout(
          title = paste("Balkendiagramm für", input$variable),
          xaxis = list(title = input$variable),
          yaxis = list(title = "Häufigkeit")
        )
    } else {
      NULL
    }
  })
  
  output$pie_chart <- renderPlotly({
    req(input$variable)
    var_data <- data[[input$variable]]
    
    if (is.factor(var_data) || is.character(var_data)) {
      plot_data <- as.data.frame(table(var_data))
      colnames(plot_data) <- c("Category", "Count")
      
      plot_ly(
        data = plot_data,
        labels = ~Category,
        values = ~Count,
        type = "pie",
        textinfo = "label+percent",
        marker = list(colors = c("lightblue", "orange", "green", "purple"))
      ) %>%
        layout(
          title = paste("Tortendiagramm für", input$variable)
        )
    } else {
      NULL
    }
  })
  
  # Tabelle für Demographische Daten
  output$demographic_table <- renderDataTable({
    summary_data <- data %>%
      group_by(Geschlecht) %>%
      summarise(
        Durchschnittsalter = round(mean(Alter), 1),
        Alter_bei_Diagnose = round(mean(Alter - Krankheitsdauer), 1),
        DurchschnittlicheKrankheitsdauer = round(mean(Krankheitsdauer), 1),
        DurchschnittlicheDepression = round(mean(Depression), 1)
      )
    datatable(summary_data, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # Korrelationen: Heatmap und Scatterplot
  output$correlation_heatmap <- renderPlot({
    req(input$correlation_vars)
    numeric_data <- data[, input$correlation_vars, drop = FALSE]
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black")
  })
  
  output$scatterplot_correlation <- renderPlot({
    ggplot(data, aes(x = Fear_of_Progression, y = Lebensqualität)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(
        title = "Korrelation: Fear of Progression vs. Lebensqualität",
        x = "Fear of Progression",
        y = "Lebensqualität"
      )
  })
  
  # Regression
  output$regression_plot <- renderPlot({
    req(input$regression_vars)
    formula <- as.formula(
      paste("Krankheitsdauer ~", paste(input$regression_vars, collapse = " + "))
    )
    fit <- lm(formula, data = data)
    
    ggplot(data, aes_string(x = input$regression_vars[1], y = "Krankheitsdauer")) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "red") +
      labs(
        title = paste("Regression: Krankheitsdauer ~", paste(input$regression_vars, collapse = " + ")),
        x = input$regression_vars[1],
        y = "Krankheitsdauer"
      )
  })
  
  # Textanalyse: Wordcloud
  output$wordcloud <- renderPlot({
    wordcloud(data$Text, max.words = 100, colors = brewer.pal(8, "Dark2"))
  })
}


# App starten
shinyApp(ui, server)


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
  reshape2       #install
  #  RColorBrewe,   #install
  #  BiocManager.    #install
)
# Lade die notwendigen Pakete
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Dummy-Datensatz
dummy_fop <- data.frame(
  age = sample(30:80, 50, replace = TRUE),
  age_diagnose_pd = sample(40:75, 50, replace = TRUE),
  sex = sample(c("Male", "Female"), 50, replace = TRUE),
  marital_status = sample(c("Single", "Married", "Divorced", "Widowed"), 50, replace = TRUE),
  education = sample(8:20, 50, replace = TRUE),
  updrs_ii_total = sample(0:40, 50, replace = TRUE),
  updrs_iii_total = sample(0:40, 50, replace = TRUE),
  paf_sum4 = sample(20:80, 50, replace = TRUE),
  years_of_illness = sample(1:30, 50, replace = TRUE),
  disease_stage = sample(1:5, 50, replace = TRUE),
  hads_grading = sample(c("Normal", "Mild", "Moderate", "Severe"), 50, replace = TRUE),
  pdq_8_index = round(runif(50, 0, 1), 2)
)

# Berechnungen der Kennzahlen
percentage_over_52 <- dummy_fop %>%
  summarise(
    total = n(),
    count_over_52 = sum(paf_sum4 > 52, na.rm = TRUE),
    percentage_over_52 = (count_over_52 / total) * 100
  )

parkinson_inzidenz_deutschland <- 200  # Beispielwert für Inzidenz in Deutschland (Anpassung je nach Quelle)

# UI-Teil der App
ui <- fluidPage(
  
  # Titel der App
  titlePanel("Parkinson Disease Data"),
  
  # Statistische Felder
  fluidRow(
    column(width = 4,
           div(class = "well",
               h4("Fear of Progression"),
               p(paste0(round(percentage_over_52$percentage_over_52, 1), "%"))
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Incidence of Parkinson's disease in Germany"),
               p(paste0(parkinson_inzidenz_deutschland, " per 100.000"))
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Average age at diagnosis"),
               p(paste0(round(mean(dummy_fop$age_diagnose_pd), 1), " years"))
           )
    )
  ),
  # Navigation Bar
  navbarPage("Navigation",
             
             # Demographic Data Section
             tabPanel("Demographic Data",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("sex", "Select Gender", choices = c("All", "Male", "Female"), selected = "All"),
                          sliderInput("age_range", "Select Age Range", min = min(dummy_fop$age), max = max(dummy_fop$age),
                                      value = c(min(dummy_fop$age), max(dummy_fop$age)), step = 1)
                        ),
                        mainPanel(
                          DTOutput("data_table")
                        )
                      )
             ),
             
             # Correlation Section
             tabPanel("Correlation",
                      sidebarLayout(
                        sidebarPanel(
                          # Hier könntest du Filter oder Auswahlmöglichkeiten für Korrelationsanalysen hinzufügen
                          textOutput("correlation_info")
                        ),
                        mainPanel(
                          plotOutput("correlation_plot")
                        )
                      )
             ),
             
             # Regression Section
             tabPanel("Regression",
                      sidebarLayout(
                        sidebarPanel(
                          # Hier kannst du Auswahlmöglichkeiten für Regressionsmodelle hinzufügen
                          textOutput("regression_info")
                        ),
                        mainPanel(
                          plotOutput("regression_plot")
                        )
                      )
             ),
             
             # Interviews Section
             tabPanel("Interviews",
                      sidebarLayout(
                        sidebarPanel(
                          # Platz für Filter oder Auswahl von Interviewdaten
                          textOutput("interviews_info")
                        ),
                        mainPanel(
                          DTOutput("interviews_table")
                        )
                      )
             )
  )
)

# Serverlogik der App
server <- function(input, output, session) {
  
  # Reaktives Filtern der Daten basierend auf Benutzereingaben
  filtered_data <- reactive({
    data <- dummy_fop
    if (input$sex != "All") {
      data <- data[data$sex == input$sex, ]
    }
    data <- data[data$age >= input$age_range[1] & data$age <= input$age_range[2], ]
    data
  })
  
  # Ausgabe der gefilterten Daten als Tabelle im Demographic Data Bereich
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  # Beispiel für eine Korrelation (z.B. zwischen UPDRS-II und UPDRS-III Scores)
  output$correlation_info <- renderText({
    "Hier können Korrelationen zwischen verschiedenen Variablen angezeigt werden."
  })
  
  output$correlation_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = updrs_ii_total, y = updrs_iii_total)) +
      geom_point() +
      labs(title = "Correlation between UPDRS-II and UPDRS-III Scores", x = "UPDRS-II", y = "UPDRS-III")
  })
  
  # Beispiel für eine Regression
  output$regression_info <- renderText({
    "Hier kann ein Regressionsmodell zwischen verschiedenen Variablen erstellt werden."
  })
  
  output$regression_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = years_of_illness, y = updrs_ii_total)) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Regression: UPDRS-II vs. Years of Illness", x = "Years of Illness", y = "UPDRS-II Score")
  })
  
  # Beispiel für Interviewdaten
  output$interviews_info <- renderText({
    "Hier können Interviewdaten angezeigt werden."
  })
  
  output$interviews_table <- renderDT({
    datatable(filtered_data())
  })
  
}

# Start der Shiny-App
shinyApp(ui = ui, server = server)


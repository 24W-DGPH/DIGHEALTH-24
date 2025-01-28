


#load packages -------                      # muss noch bereinigt werden
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
  wordcloud2,      #install
  plotly,         #install Darstellung interaktive Balken-/Tortendiagramme
  reshape2,       #install
  rsconnect,      #install
)

# Benötigte Pakete laden
library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(corrplot)
library(tm)
library(wordcloud2)
library(reshape2)
library(rsconnect)


#import dataset ------

#Datenimport überprüfen
cat("Aktuelles Arbeitsverzeichnis: ", getwd(), "\n")
cat("Datei existiert: ", file.exists("./Progredienzangst.sav"), "\n")


data <- read_sav("./Progredienzangst.sav")
FoP_spss <- data

source("CleanDataFoP.R")

#App--------

# UI-Teil----
ui <- fluidPage(
  titlePanel(h1("Fear of Progression in Parkinson's Patients")),       #Überschrift
  
  #Statistische Felder mit Kennzahlen/Definition
  
  fluidRow(
    column(width = 4,
           div(class = "well",
               h4("Fear of Progression"),                                 #Berechnung prozentualer Anteil der Probanden mit einem PAFSummenwert >52, dieser in der Literatur als Grenzwert angegeben
               p(paste0({
                 percentage_over_52 <- clean_FoP_spss %>%
                   summarise( total = n(), count_over_52 = sum(paf_sum4 > 52, na.rm = TRUE),
                              percentage_over_52 = (count_over_52/total)*100 )
                 paste0(round(percentage_over_52$percentage_over_52, 1), "%")
               }))
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Incidence of Parkinson's disease in Germany"),
               p("112 per 100.000")                                          # laut Recherche aktuelle Inzidenz in Deutschland
           )
    ),
    column(width = 4,                                                       # Berechnung Durchschnittsalter bei Diagnose
           div(class = "well",
               h4("Average age at diagnosis"),
               p(paste0(round(mean(clean_FoP_spss$age_diagnose_pd), 1), " years"))
           )
    )
  ),
  
  fluidRow(
    column( width =12,
            div(class = "well",
                h3("Fear of Progression Definition"),
                p("Fear of progression (FoP) refers to the fear of disease progression
                  and its psychosocial consequences. It is a reactive and consciously 
                  perceived fear arising from the experience of a severe, potentially 
                  life-threatening, or disabling illness and its treatment. The predictive 
                  factors and impact of FoP vary depending on the underlying condition, leading 
                  to different overall anxiety levels and patterns. FoP may emerge directly 
                  after diagnosis or during the illness. While it is a normal reaction to a life-altering 
                  diagnosis and can lead to adaptive behaviors, it may also cause dysfunctional 
                  responses that impair the quality of life and require treatment. General 
                  mental and somatic anxiety symptoms are central, with their severity 
                  correlating to the intensity of the fear.")
                )
            )
  ),
  
  #Navigationsleiste----
  
  # Navigationsleiste
  navbarPage("",
             tabPanel("Demographic Data",           #Demographische Daten
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var_select", "Select Variable for Plot:",                                   #Auswählen Variablen, für die Säulen-/Balkendiagramme erstellt werden sollen
                                      choices = setNames(variable_mapping1$variable,variable_mapping1$synonym)),
                          sliderInput("age_range", "Select Age Range:",                                            #Darstellung durch individuell gewählte Altergruppen möglich
                                      min = min(clean_FoP_spss$age), max = max(clean_FoP_spss$age),
                                      value = c(min(clean_FoP_spss$age), max(clean_FoP_spss$age)),
                                      step = 1),
                          selectInput("dist_var", "Select Variable for Distribution Plot:",                        #Auswählen Variablen für die ein Verteilungsdiagramm erstellt werden soll
                                      choices = setNames(variable_mapping2$variable,variable_mapping2$synonym)),
                        
                          #Erklärung
                          HTML("Demographic data is presented first in studies to provide                           
                          essential context about the study population. It helps describe participant 
                          characteristics, assess the sample's representativeness, and enable comparisons with other 
                          studies. Additionally, demographic variables often act as confounding factors and need to be
                          identified early to ensure accurate interpretation of results. This enhances transparency, 
                          credibility, and the ability to generalize findings.The role of age in the manifestation of 
                          FoP is debated in the literature, making it a critical factor for analysis. Some studies suggest 
                          that older individuals may experience less pronounced FoP due to greater life experience and a more 
                          realistic understanding of health risks, potentially as a protective factor. Conversely, others 
                          argue that older age might increase FoP due to higher exposure to chronic or severe illnesses, 
                          reduced social support, and physical limitations. Analysing age within the dataset is interesting
                          in clarifying its impact and exploring whether it acts as a protective or risk factor for FoP 
                          in the study population."),
                          ),
                        
                        mainPanel(fluidRow(                                        #Anzeigen der Diagramme Säulen/Balkendiagramm,Verteilungsdiagramm,Tabelle
                          column(6, plotOutput("bar_plot")),
                          column(6, plotOutput("pie_chart")),
                        ),
                        plotOutput("dist_plot"),  
                        dataTableOutput("demographic_table")
                        )
                      )
             ),
             tabPanel("Correlation",               #Korrelationen
                      sidebarLayout(
                        sidebarPanel(                                                            #Auswahloptionen für Variablen
                          checkboxGroupInput("corr_vars", "Select Variables for Correlation:",
                                             choices = setNames(variable_mapping3$variable,variable_mapping3$synonym)),
                          helpText("Note: 'FoP' should always be included in the correlation."),
                          
                          #Erklärung
                          HTML("Before performing a regression analysis, a correlation analysis is                      
                                  typically conducted in statistical evaluations to identify potential relationships 
                                  between variables and to determine their strength and direction. This step is useful
                                  as it ensures that only variables with potentially relevant relationships to the 
                                  dependent variable are included in the regression analysis. Additionally, the correlation 
                                  analysis can reveal problematic multicollinearity between independent variables, which could 
                                  distort the regression results. This creates a solid foundation for a more precise 
                                  and meaningful regression model."),
                           ),
                      
                        mainPanel(fluidRow(                       #Anzeigen Heatmap (variabel) und Scatternplot
                          column(width = 12,
                                 plotOutput("heatmap_plot"),
                                 plotOutput("scatterplot_paf_qol", height = "400px")
                          )
                        )
                        )
                      )
             ),
             tabPanel("Regression",
                      sidebarLayout(                                                           # Auswahl des Regressionsmodells durch den Nutzer
                        sidebarPanel(selectInput("selected_model", "Choose a Regression Model", 
                                                 choices = c("Model 1", "Model 2", "Model 3")),
                                     helpText("Note: Choose a regression model to run."),
                                    
                        #Erklärung
                        HTML("The clinical relevance lies in the future need for early                     
                        identification of patients at high risk for FoP (Fear of Progression) to avoid 
                        misdiagnoses such as depression or anxiety disorders. This early detection would 
                        also help prevent the pathological progression of anxiety and protect the patients’ 
                                     relatives from similar risks. Sociodemographic factors, psychological preconditions, 
                                     and low health literacy are suspected as potential risk or triggering factors. Early 
                                     detection promises significant individual benefits for patients by reducing their 
                                     daily life impairments. Through the development of more targeted and specific 
                                     psychotherapeutic interventions, quality of life can be preserved or improved.To 
                                     identify high-risk patients, it is crucial to examine more specific influencing 
                                     factors. Regression analysis plays a vital role here by determining predictors—based 
                                     on proven correlations—among sociodemographic, neuropsychological, and clinical 
                                     variables, as well as their predictive power. This enables a more precise 
                                     understanding of risk factors and supports the creation of effective preventative 
                                     strategies."),
                                   
                                     
                        ),
                        mainPanel(plotOutput("regression_plot"),                     # Regressionsgraph
                                  verbatimTextOutput("regression_results")           # Ausgabe der Regressionsparameter Hinweis: Erscheinen nicht in der App, da kein Ergebnis mit den zufällig erstellten Modellen berechnet werden kann)
                        )),
                      ),
             
             tabPanel("Interviews",                  #Interviews, aktuell Dummyvariablen
                      #sidebarLayout(
                      #  sidebarPanel("You see a interactive Wordcloud based on the statements of the patients"),
                      #  mainPanel(wordcloud2Output("wordcloud", width = "100%", height = "800px"))
                      #)
                      
                      fluidPage(
                        titlePanel(h4("Wordcloud- Interviews")), 
                        wordcloud2Output("wordcloud", width = "120%", height = "1000px")  # Große Wordcloud
                      )
                     )
  )
)
  


#Server-----

server <- function(input, output, session) {

  #Demographsiche Daten-----  
  # Reaktive Datenfiltrierung
  filtered_data <- reactive({
    data <- clean_FoP_spss
    data <- data[data$age >= input$age_range[1] & data$age <= input$age_range[2], ]   # FilterAlter für Slider
    return(data)
  })
  
  # Berechne die demografische Tabelle für ausgewählte Variablen.    Hier muss die Anzeige der Variablen und Beschriftungen noch verbessert werden, Farben usw.
  output$demographic_table <- renderDataTable({
    summary_data <- data.frame(
      Variable = c("Age", "Years of Education", 
                   "Age at Diagnosis", "UPDRS II Total", "UPDRS III Total"),
      N = c(sum(!is.na(filtered_data()$age)),
            sum(!is.na(filtered_data()$years_education)),
            sum(!is.na(filtered_data()$age_diagnose_pd)),
            sum(!is.na(filtered_data()$updrs_ii_total)),
            sum(!is.na(filtered_data()$updrs_iii_total))),
      Median = c(median(filtered_data()$age, na.rm = TRUE),
                 median(filtered_data()$years_education, na.rm = TRUE),
                 median(filtered_data()$age_diagnose_pd, na.rm = TRUE),
                 median(filtered_data()$updrs_ii_total, na.rm = TRUE),
                 median(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Mean = c(mean(filtered_data()$age, na.rm = TRUE),
               mean(filtered_data()$years_education, na.rm = TRUE),
               mean(filtered_data()$age_diagnose_pd, na.rm = TRUE),
               mean(filtered_data()$updrs_ii_total, na.rm = TRUE),
               mean(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      SD = c(sd(filtered_data()$age, na.rm = TRUE),
             sd(filtered_data()$years_education, na.rm = TRUE),
             sd(filtered_data()$age_diagnose_pd, na.rm = TRUE),
             sd(filtered_data()$updrs_ii_total, na.rm = TRUE),
             sd(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Min = c(min(filtered_data()$age, na.rm = TRUE),
              min(filtered_data()$years_education, na.rm = TRUE),
              min(filtered_data()$age_diagnose_pd, na.rm = TRUE),
              min(filtered_data()$updrs_ii_total, na.rm = TRUE),
              min(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Max = c(max(filtered_data()$age, na.rm = TRUE),
              max(filtered_data()$years_education, na.rm = TRUE),
              max(filtered_data()$age_diagnose_pd, na.rm = TRUE),
              max(filtered_data()$updrs_ii_total, na.rm = TRUE),
              max(filtered_data()$updrs_iii_total, na.rm = TRUE))
    )
    
    # Gib die berechneten Statistiken zurück
    datatable(summary_data, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # Balkendiagramm
  output$bar_plot <- renderPlot({
    selected_var <- input$var_select
    
    #Sicherstellen, dass die ausgewählte Variable existiert
    if (selected_var %in% colnames(filtered_data())) {
      var_synonym <- variable_mapping1$synonym[variable_mapping1$variable == selected_var]
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_bar() +
        labs(title = paste("Bar Plot of", var_synonym), x = var_synonym, y = "Count")}
  })
  
  # Tortendiagramm
  output$pie_chart <- renderPlot({
    selected_var <- input$var_select
    
    if (selected_var %in% colnames(filtered_data())) {       #Sicherstellen, dass die ausgewählte Variable existiert
      var_data <- filtered_data()[[selected_var]]
      var_synonym <- variable_mapping1$synonym[variable_mapping1$variable == selected_var]
      var_table <- table(var_data)
      var_percentage <- prop.table(var_table) * 100
      var_percentage <- var_percentage[var_percentage > 0]       #Werte mit 0% entfernen
      pie(var_percentage, labels = paste(names(var_percentage), round(var_percentage, 1), "%"), 
          main = paste("Pie Chart of", var_synonym))}
  })
  
  #Verteilungsdiagramm
  output$dist_plot <- renderPlot({
    selected_var <- input$dist_var
    
    if (selected_var %in% colnames(filtered_data())) {     #Sicherstellen, dass die ausgewählte Variable existiert
      var_synonym <- variable_mapping2$synonym[variable_mapping2$variable == selected_var]
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Distribution of", var_synonym), x = var_synonym, y = "Frequency")}
  })
  
  #Korrelation/HeatMap-----
  
  #Auswahl der Variablen für die Heatmap
  selected_corr_data <- reactive({
    selected_vars <- input$corr_vars
    
    existing_vars <- intersect(selected_vars, colnames(clean_FoP_spss))      # Prüfen,ob die Spalten im Datensatz existieren, falls nicht Anzeige
    if (length(existing_vars) == 0) { showNotification("No valid columns selected or columns not present in the dataset!", type = "error")
      return(NULL)}
    
    filtered_data <- clean_FoP_spss[, existing_vars, drop = FALSE]       # Filtert den Datensatz basierend auf den vorhandenen Variablen
    
    #Numerische Variablen umwandeln (z. B. Faktorvariablen in Zahlen),eigentl. überflüssig, werden nur numerische Variablen angeboten
    filtered_data <- filtered_data %>%
      mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
      na.omit()                                                           # Entfernen von NA-Werten für die Korrelation
    
    return(filtered_data)
  })
  
  #Heatmap-Plot
  output$heatmap_plot <- renderPlot({
    corr_data <- selected_corr_data()
    
    if (is.null(corr_data) || ncol(corr_data) < 2) {           # Überprüfung, ob Daten vorhanden sind
      showNotification("Not enough data to create the heatmap!", type = "error")
      return(NULL)}
    
    #Berechnung der Korrelation
    corr_matrix <- cor(corr_data, use = "complete.obs")
    
    #Umwandlung in ein Datenframe für ggplot
    corr_melt <- as.data.frame(as.table(corr_matrix))
    colnames(corr_melt) <- c("Var1", "Var2", "Correlation")
    
    #Variablennamen ersetzen
    corr_melt$Var1 <- variable_mapping3$synonym[match(corr_melt$Var1, variable_mapping3$variable)]
    corr_melt$Var2 <-  variable_mapping3$synonym[match(corr_melt$Var2,variable_mapping3$variable)]
    
    #Heatmap mit ggplot2.    Farben ändern!
    ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "lightblue", mid = "white", high = "midnightblue", midpoint = 0,
                           limits = c(-1, 1), name = "Correlation") +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Scatterplot für PaF und Lebensqualität berechnen
  output$scatterplot_paf_qol <- renderPlot({
    plot_data <- clean_FoP_spss %>%                           
      select(paf_sum4, pdq_8_total) %>%
      na.omit()
    
    if (nrow(plot_data) == 0) {               # Überprüfe, ob Daten vorhanden sind
      showNotification("No data available for the scatter plot!", type = "error")
      return(NULL)}
    
    #Scatterplot erstellen.        hier müssenebenfalls die Anzeige Variablen noch geändert werden
    ggplot(plot_data, aes(x = paf_sum4, y = pdq_8_total)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) + 
      labs(
        title = "Scatterplot: Fear of Progression vs. Quality of Life",
        x = "Fear of Progression (PaF)",
        y = "Quality of Life (PDQ-8)"
      ) +
      theme_minimal()
  })
  
  #Regression---
  
  # Regressionsmodell und Plot-Generierung
  output$regression_plot <- renderPlot({
    
    #Auswahl des Regressionsmodells durch den Nutzer
    selected_model <- input$selected_model               #Auswahl von 3 Modellen
    
    if (is.null(selected_model)) {        # Überprüfen, ob das Modell ausgewählt wurde
      showNotification("Please select a regression model", type = "error")
      return(NULL)}
    
    #Definieren der Variablen für jedes Modell
    if (selected_model == "Model 1") {
      selected_vars <- c("age", "age_diagnose_pd", "education", "disease_stage", "years_of_illness", "hads_d_total")
    } else if (selected_model == "Model 2") {
      selected_vars <- c("age", "disease_stage", "years_of_illness", "hads_d_total")
    } else if (selected_model == "Model 3") {
      selected_vars <- c("age", "education", "hads_d_total")
    } else {
      showNotification("Invalid model selection", type = "error")
      return(NULL)
    }
    
    #Umwandlung der ausgewählten Variablen in den richtigen Datensatz
    regression_data <- clean_FoP_spss %>%
      select(paf_sum4, all_of(selected_vars)) %>%
      na.omit()                                                        #Entfernen von NA-Werten
    
    #Überprüfen, ob nach der Auswahl der Variablen noch Daten vorhanden sind
    if (nrow(regression_data) == 0) {
      showNotification("No data available for the selected variables", type = "error")
      return(NULL)}
    
    #Erstellen des Regressionsmodells mit den ausgewählten Variablen
    regression_formula <- as.formula(paste("paf_sum4 ~", paste(selected_vars, collapse = " + ")))
    regression_model <- lm(regression_formula, data = regression_data)
    
    # Berechnen der Vorhersagewerte (fitted values)
    fitted_values <- predict(regression_model, newdata = regression_data)
    
    # Fügen Sie die Vorhersagewerte zum Datensatz hinzu
    regression_data$fitted_values <- fitted_values
    
    # Überprüfen, ob fitted_values berechnet wurden
    if (length(fitted_values) == 0) {
      showNotification("Error calculating fitted values", type = "error")
      return(NULL)}
    
    x_synonym <- variable_mapping4$synonym[variable_mapping4$variable == "paf_sum4"]
    selected_vars_synonyms <- variable_mapping4$synonym[match(selected_vars,variable_mapping4$variable)]
    
    #Erstellen des Scatterplots
    ggplot(regression_data, aes(x = paf_sum4, y = fitted_values)) +
      geom_point(aes(color = paf_sum4), alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = paste("Regression: PaF vs.", paste(selected_vars_synonyms, collapse = ", ")), 
           x = "Fear of Progression (PaF)", y = "Predicted PaF") +
      theme_minimal()
  });
  
  #Regressionsergebnisse als Text ausgeben
  output$regression_results <- renderPrint({
    # Falls das Modell leer ist, nichts ausgeben
    if (exists("regression_model")) {
      summary(regression_model)
    }
  });
  
  #Interviews---- Darstellen als WordCloud
  
  output$wordcloud <- renderWordcloud2({
    
    #zusätzliche Stoppwörter aussortieren
    additional_stopwords <- c("dass", "und", "oder", "ist", "es", "nicht", "ich", "wie", "das", 
                              "die", "der", "den", "ein", "eine", "sich", "kann", "schon", 
                              "noch", "wird", "muss", "meine", "mir", "für")
    
  
    #Textdaten für die Wordcloud extrahieren
    text_data <- clean_FoP_spss$Statement
    
    #Textdaten reinigen
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("de"))
    corpus <- tm_map(corpus, removeWords, additional_stopwords) #zusätzlich definierte Wörter entfernen
    corpus <- tm_map(corpus, stripWhitespace)
    
    #Erstellen einer Term-Document-Matrix
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    #Erstellen eines Datenrahmens mit den Wörtern und Häufigkeiten
    word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    #Generierung der Wordcloud
    wordcloud2(word_data, size = 1,7, color = "random-light", backgroundColor = "white")
  })
}

#packrat, rsconnect
#rsconnect::deployApp(".")
#rsconnect::setAccountInfo(name='datavisualisationwendland', 
 #                         token='60D13FDEF099677EA6C16064751B0B10', 
  #                        secret='wX8fRUIYiX1Xrh4dCECvVgwHuDiFoaqkwHvJ4Jgp')

#rsconnect::accounts()


#rsconnect::deployApp(appDir = ".", appName = "FearofProgressionApp", forceUpdate = TRUE)

# App starten
shinyApp(ui = ui, server = server)

# richtiger link: https://datavisualisationwendland.shinyapps.io/FearOfProgressionApp/
# https://wendwiczdatavisualisation.shinyapps.io/FearOfProgression/
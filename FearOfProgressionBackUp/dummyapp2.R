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
  wordcloud2,      #install
  plotly,         #install Darstellung interaktive Balken-/Tortendiagramme
  reshape2       #install
  #  RColorBrewe,   #install
  #  BiocManager.    #install
)
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(wordcloud2)

# Beispiel Datensatz (Dummy-Daten)
dummy_data <- data.frame(
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
  pdq_8_index = round(runif(50, 0, 1), 2),
  hads_d_total = sample(0:21, 50, replace = TRUE),
  pdq_8_total = sample(0:100, 50, replace = TRUE)
)

# Dein neuer Datensatz mit den Interview-Statements
patient_statements <- data.frame(
  Statement = c(
    "Ich habe ständig Angst, dass meine Krankheit schlimmer wird und ich irgendwann nicht mehr für meine Familie sorgen kann.",
    "Es macht mir Sorgen, dass ich irgendwann nicht mehr selbstständig leben kann, das beeinflusst meine Lebensqualität.",
    "Ich fürchte, dass ich meine Arbeit nicht mehr ausführen kann und das meinen Lebensunterhalt gefährdet.",
    "Die Vorstellung, dass meine Erkrankung meine Angehörigen belastet, lässt mich nicht los.",
    "Ich mache mir ständig Gedanken darüber, wie meine Krankheit die Zukunft meiner Kinder beeinflussen könnte.",
    "Es belastet mich, dass ich nicht mehr so aktiv sein kann wie früher, und dass ich mein soziales Leben verliere.",
    "Ich habe Angst, dass die Krankheit so fortschreitet, dass ich irgendwann im Krankenhaus oder Pflegeheim landen muss.",
    "Jeden Tag frage ich mich, ob ich meine eigenen Bedürfnisse noch erfüllen kann, ohne auf Hilfe angewiesen zu sein.",
    "Ich habe oft das Gefühl, dass ich meine Familie mit meiner Krankheit erdrücke.",
    "Die Unsicherheit darüber, was die Zukunft bringt, macht es mir schwer, optimistisch zu bleiben.",
    "Ich fürchte, dass ich aufgrund meiner Erkrankung nicht mehr die Kontrolle über mein Leben haben werde.",
    "Ich mache mir Sorgen, dass die Symptome bald so schlimm werden, dass ich mich nicht mehr um meine eigenen Angelegenheiten kümmern kann.",
    "Es fällt mir schwer, an die Zukunft zu denken, weil ich nicht weiß, wie sich meine Krankheit entwickeln wird.",
    "Ich habe Angst, dass die Krankheit mich irgendwann so stark einschränkt, dass ich meinen Hobbys und Interessen nicht mehr nachgehen kann.",
    "Die Vorstellung, dass ich meine Lebensqualität verliere, macht mir oft Angst."
  )
)

# Zufällig 50 Statements auswählen, um die gleiche Länge wie der andere Datensatz zu haben
set.seed(42)  # Für Reproduzierbarkeit
patient_statements <- patient_statements[sample(1:nrow(patient_statements), 50, replace = TRUE), , drop = FALSE]

# Zusammenführen des Datensatzes mit den Statements und den ursprünglichen Daten
dummy_fop <- cbind(dummy_data, patient_statements)


# UI-Teil----
ui <- fluidPage(
  titlePanel("Statistische Analysen von Parkinson-Daten"),
  
#CSS----  
  # Hinzufügen von CSS, um die Wordcloud am oberen Rand auszurichten
  tags$head(
    tags$style(HTML("
      #wordcloud {
        border: 5px solid #000;  /* Schwarzer Rahmen */
        padding: 10px;            /* Abstand innerhalb des Rahmens */
        background-color: #f8f8f8; /* Hellgrauer Hintergrund */
        border-radius: 15px;      /* Abgerundete Ecken */
        margin-top: 0;            /* Kein Abstand nach oben */
        margin-bottom: 0;         /* Kein Abstand nach unten */
        position: relative;       /* Positionierung innerhalb des Containers */
        top: 0;                   /* An den oberen Rand des Containers verschieben */
      }
    "))
  ),
  
  #navigationsleiste ----
  # Kennzahlen über der Navigationsleiste
  fluidRow(
    column(width = 4,
           div(class = "well",
               h4("Fear of Progression"),
               p(paste0({
                 percentage_over_52 <- dummy_fop %>%
                   summarise( total = n(), count_over_52 = sum(paf_sum4 > 52, na.rm = TRUE),
                              percentage_over_52 = (count_over_52/total)*100 )
                 paste0(round(percentage_over_52$percentage_over_52, 1), "%")
               }))
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Incidence of Parkinson's disease in Germany"),
               p("X per 100.000")  # Hier eine Zahl nach Bedarf einfügen
           )
    ),
    column(width = 4,
           div(class = "well",
               h4("Average age at diagnosis"),
               p(paste0(round(mean(dummy_fop$age_diagnose_pd), 1), " years"))
           )
    )
  ),
  
  # Navigationsleiste
  navbarPage("",
              tabPanel("Demographic Data",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var_select", "Select Variable for Plot:",
                                      choices = c("sex", "years_of_illness", "disease_stage", "marital_status", 
                                                  "education", "paf_sum4", "hads_grading")),
                          sliderInput("age_range", "Select Age Range:",
                                      min = min(dummy_fop$age), max = max(dummy_fop$age),
                                      value = c(min(dummy_fop$age), max(dummy_fop$age)),
                                      step = 1),
                          selectInput("dist_var", "Select Variable for Distribution Plot:",
                                      choices = c("age", "age_diagnose_pd", "years_of_illness", "updrs_ii_total", 
                                                  "updrs_iii_total", "paf_sum4", "hads_grading", "pdq_8_index"))
                        ),
                        
                        mainPanel(
                          
                          fluidRow(
                            column(6, plotOutput("bar_plot")),
                            column(6, plotOutput("pie_chart")),
                          ),
                          
                          plotOutput("dist_plot"),  # Verteilungsdiagramm
                          dataTableOutput("demographic_table")
                        )
                      )
             ),
             
             tabPanel("Correlation",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput("corr_vars", "Select Variables for Correlation:",
                                             choices = c("paf_sum4", "age", "years_of_illness", "hads_d_total", 
                                                         "disease_stage", "pdf_8_total"),
                                             selected = c("paf_sum4", "age", "years_of_illness", "hads_d_total")),
                          helpText("Note: 'paf_sum4' should always be included in the correlation.")
                        ),
                        mainPanel(
                          fluidRow(
                            column(width = 12,
                                   plotOutput("heatmap_plot"),
                                   plotOutput("scatterplot_paf_qol", height = "400px")
                            )
                          )
                        )
                      )
             ),
             
             tabPanel("Regression",
                      sidebarLayout(
                        sidebarPanel(
                          # Auswahl des Regressionsmodells durch den Nutzer
                          selectInput("selected_model", "Choose a Regression Model", 
                                      choices = c("Model 1", "Model 2", "Model 3")),
                          
                          helpText("Note: Choose a regression model to run.")
                        ),
                        mainPanel(
                          plotOutput("regression_plot"),
                          verbatimTextOutput("regression_results")  # Ausgabe der Regressionsparameter
                        )
                      )
             ),
             
             tabPanel("Interviews",
                      sidebarLayout(
                        sidebarPanel("You see a interactive Wordcloud based on the statements of the patients"),
                        mainPanel(wordcloud2Output("wordcloud", width = "100%", height = "800px"))
                      )
             )
  )
)

# Server-Teil-----
server <- function(input, output, session) {
  
  # Reaktive Datenfiltrierung
  filtered_data <- reactive({
    data <- dummy_fop
    
    # Filter nach Geschlecht und Alter
    data <- data[data$age >= input$age_range[1] & data$age <= input$age_range[2], ]
    
    return(data)
  })
  
  # Berechne die demografische Tabelle
  output$demographic_table <- renderDataTable({
    summary_data <- data.frame(
      Variable = c("Age", "Sex", "Marital Status", "Years of Education", 
                   "Age at Diagnosis", "UPDRS II Total", "UPDRS III Total"),
      N = c(sum(!is.na(filtered_data()$age)),
            sum(!is.na(filtered_data()$sex)),
            sum(!is.na(filtered_data()$marital_status)),
            sum(!is.na(filtered_data()$education)),
            sum(!is.na(filtered_data()$age_diagnose_pd)),
            sum(!is.na(filtered_data()$updrs_ii_total)),
            sum(!is.na(filtered_data()$updrs_iii_total))),
      Median = c(median(filtered_data()$age, na.rm = TRUE),
                 NA, 
                 NA,
                 median(filtered_data()$education, na.rm = TRUE),
                 median(filtered_data()$age_diagnose_pd, na.rm = TRUE),
                 median(filtered_data()$updrs_ii_total, na.rm = TRUE),
                 median(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Mean = c(mean(filtered_data()$age, na.rm = TRUE),
               NA,  
               NA,
               mean(filtered_data()$education, na.rm = TRUE),
               mean(filtered_data()$age_diagnose_pd, na.rm = TRUE),
               mean(filtered_data()$updrs_ii_total, na.rm = TRUE),
               mean(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      SD = c(sd(filtered_data()$age, na.rm = TRUE),
             NA,  
             NA,
             sd(filtered_data()$education, na.rm = TRUE),
             sd(filtered_data()$age_diagnose_pd, na.rm = TRUE),
             sd(filtered_data()$updrs_ii_total, na.rm = TRUE),
             sd(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Min = c(min(filtered_data()$age, na.rm = TRUE),
              NA,  
              NA,
              min(filtered_data()$education, na.rm = TRUE),
              min(filtered_data()$age_diagnose_pd, na.rm = TRUE),
              min(filtered_data()$updrs_ii_total, na.rm = TRUE),
              min(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Max = c(max(filtered_data()$age, na.rm = TRUE),
              NA,  
              NA,
              max(filtered_data()$education, na.rm = TRUE),
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
    
    # Sicherstellen, dass die ausgewählte Variable existiert
    if (selected_var %in% colnames(filtered_data())) {
      
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_bar() +
        labs(title = paste("Bar Plot of", selected_var), x = selected_var, y = "Count")
    }
  })
  
  # Tortendiagramm
  output$pie_chart <- renderPlot({
    selected_var <- input$var_select
    
    # Sicherstellen, dass die ausgewählte Variable existiert
    if (selected_var %in% colnames(filtered_data())) {
      
      var_data <- filtered_data()[[selected_var]]
      var_table <- table(var_data)
      var_percentage <- prop.table(var_table) * 100
      pie(var_percentage, labels = paste(names(var_percentage), round(var_percentage, 1), "%"), 
          main = paste("Pie Chart of", selected_var))
    }
  })
  
  # Verteilungsdiagramm
  output$dist_plot <- renderPlot({
    selected_var <- input$dist_var
    
    # Sicherstellen, dass die ausgewählte Variable existiert
    if (selected_var %in% colnames(filtered_data())) {
      
      # Wählen Sie den passenden Plot basierend auf der Variable aus
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Distribution of", selected_var), x = selected_var, y = "Frequency")
    }
  })
  
  
  # Reaktive Funktion: Auswahl der Variablen für die Heatmap
  selected_corr_data <- reactive({
  
    selected_vars <- input$corr_vars
    
    # Prüfen, ob die Spalten im Datensatz existieren
    existing_vars <- intersect(selected_vars, colnames(dummy_fop))
    if (length(existing_vars) == 0) {
      showNotification("Keine gültigen Spalten ausgewählt oder Spalten nicht im Datensatz vorhanden!", type = "error")
      return(NULL)
    }
    
    # Filtere den Datensatz basierend auf den vorhandenen Variablen
    filtered_data <- dummy_fop[, existing_vars, drop = FALSE]
    
    # Numerische Variablen umwandeln (z. B. Faktorvariablen in Zahlen)
    filtered_data <- filtered_data %>%
      mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
      na.omit()  # Entfernen von NA-Werten für die Korrelation
    
    return(filtered_data)
  })
  
  # Heatmap-Plot
  output$heatmap_plot <- renderPlot({
    corr_data <- selected_corr_data()
    
    # Überprüfung, ob Daten vorhanden sind
    if (is.null(corr_data) || ncol(corr_data) < 2) {
      showNotification("Nicht genügend Daten für die Erstellung der Heatmap!", type = "error")
      return(NULL)
    }
    
    # Berechnung der Korrelation
    corr_matrix <- cor(corr_data, use = "complete.obs")
    
    # Umwandlung in ein Datenframe für ggplot
    corr_melt <- as.data.frame(as.table(corr_matrix))
    colnames(corr_melt) <- c("Var1", "Var2", "Correlation")
    
    # Heatmap mit ggplot2
    ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                           limits = c(-1, 1), name = "Correlation") +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Scatterplot für PaF und Lebensqualität
  output$scatterplot_paf_qol <- renderPlot({
    # Daten filtern (nur relevante Spalten, ohne NAs)
    plot_data <- dummy_fop %>%
      select(paf_sum4, pdq_8_total) %>%
      na.omit()
    
    # Überprüfe, ob Daten vorhanden sind
    if (nrow(plot_data) == 0) {
      showNotification("Keine Daten für den Scatterplot verfügbar!", type = "error")
      return(NULL)
    }
    
    # Scatterplot erstellen
    ggplot(plot_data, aes(x = paf_sum4, y = pdq_8_total)) +
      geom_point(color = "blue", alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) + # Lineare Trendlinie
      labs(
        title = "Scatterplot: Fear of Progression vs. Quality of Life",
        x = "Fear of Progression (PaF)",
        y = "Quality of Life (PDQ-8)"
      ) +
      theme_minimal()
  })
  
#Regression----
  
  # Regressionsmodell und Plot-Generierung
  output$regression_plot <- renderPlot({
    # Auswahl des Regressionsmodells durch den Nutzer
    selected_model <- input$selected_model  # Annahme: input$selected_model ist ein Dropdown oder ein anderes UI-Element zur Auswahl des Modells
    
    # Überprüfen, ob das Modell ausgewählt wurde
    if (is.null(selected_model)) {
      showNotification("Please select a regression model", type = "error")
      return(NULL)
    }
    
    # Definieren der Variablen für jedes Modell
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
    
    # Umwandlung der ausgewählten Variablen in den richtigen Datensatz
    regression_data <- dummy_fop %>%
      select(paf_sum4, all_of(selected_vars)) %>%
      na.omit()  # Entfernen von NA-Werten
    
    # Überprüfen, ob nach der Auswahl der Variablen noch Daten vorhanden sind
    if (nrow(regression_data) == 0) {
      showNotification("No data available for the selected variables", type = "error")
      return(NULL)
    }
    
    # Erstellen des Regressionsmodells mit den ausgewählten Variablen
    regression_formula <- as.formula(paste("paf_sum4 ~", paste(selected_vars, collapse = " + ")))
    regression_model <- lm(regression_formula, data = regression_data)
    
    # Berechnen der Vorhersagewerte (fitted values)
    fitted_values <- predict(regression_model, newdata = regression_data)
    
    # Fügen Sie die Vorhersagewerte zum Datensatz hinzu
    regression_data$fitted_values <- fitted_values
    
    # Überprüfen, ob fitted_values berechnet wurden
    if (length(fitted_values) == 0) {
      showNotification("Error calculating fitted values", type = "error")
      return(NULL)
    }
    
    # Erstellen des Scatterplots
    ggplot(regression_data, aes(x = paf_sum4, y = fitted_values)) +
      geom_point(aes(color = paf_sum4), alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = paste("Regression: PaF vs.", paste(selected_vars, collapse = ", ")), 
           x = "Fear of Progression (PaF)", y = "Predicted PaF") +
      theme_minimal()
  });
  
  # Regressionsergebnisse als Text ausgeben
  output$regression_results <- renderPrint({
    # Falls das Modell leer ist, nichts ausgeben
    if (exists("regression_model")) {
      summary(regression_model)
    }
  });
  
#Interviews------
  output$wordcloud <- renderWordcloud2({
    
    # Textdaten für die Wordcloud extrahieren
    text_data <- dummy_fop$Statement
    
    # Textdaten reinigen
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("de"))  # Stopwords auf Deutsch entfernen
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Erstellen einer Term-Document-Matrix
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    # Erstellen eines Datenrahmens mit den Wörtern und Häufigkeiten
    word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    # Generierung der Wordcloud
    wordcloud2(word_data, size = 0.5, color = "random-light", backgroundColor = "white")
  })

  
    
}


# App starten
shinyApp(ui = ui, server = server)


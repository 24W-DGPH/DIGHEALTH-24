
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
  reshape2       #install
  #  RColorBrewe,   #install
  #  BiocManager.    #install
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
#library(RColorBrewer)
#library(BiocManager)

#import dataset ------
FoP_spss <- read_sav("/Applications/Progredienzangst_SPSS_09.05.2019_final Kopie 2.sav")

#clean dataset---------

clean_FoP_spss <- FoP_spss %>%            #create clean data set clean_FoP_spss
  
  clean_names() %>%                       #stanardize syntax of column names
  rename(centre= zentrum,                 #rename variables
         date= datum,
         experimenter= versuchsleiter,
         birthday= geburtsdatum,
         age= alter,
         sex= geschlecht,
         marital_status= familienstand,
         living_situation= wohnsituation,
         education= schulabschluss,
         years_education= anzahl_schuljahre,
         profession= haupttatigkeit,
         age_diagnose_pd= alter_diagnose_pd,
         deep_brain_stimulation_yn= ths_ja_nein,
         level_of_care= pflegegrad,
         updrs_ii_total= updrs_ii_adl_gesamtwert,
         updrs_iii_levodopa_yn= updrs_iii_levodopa_ja_nein,
         updrs_iii_total= updrs_iii_gesamtwert,
         pain_scale= schmerzskala_rohwert,
         paf_sum4= paf_summenwert_4_subskalen,
         paf_grading= paf_einteilung,
         paf_sum_coping= paf_summenwert_coping,
         sci_worry_total= sci_sorgen_gesamt,
         sci_difficulties_total= sci_gesamt_schwierigkeiten_spezifisch,
         moca_total= mo_ca_gesamtwert_ohne_korrektur,
         moca_total_corrected= mo_ca_gesamtwert_mit_korrektur,
         moca_result= mo_ca_auswertung,
         hads_d_fear_sum= hads_d_angstwert_summe_a_t_wert,
         hads_d_depression_sum= hads_d_depressivitatswert_summe_d_t_wert,
         hads_d_total= hads_gesamt,
         hads_grading= hads_auswertung,
         pdq_8_total= pdq_8_gesamtwert,
         pdq_8_index= pdq8_si,
         health_literacy_total= health_literacy_gesamtwert,
         health_literacy_index= hls_index) 

#names(clean_FoP_spss) #proof new names

#modify dataset to select only necessary rows and columns
clean_FoP_spss <- clean_FoP_spss %>% 
  
  select(
    id,
    centre , 
    date, 
    experimenter,
    birthday,
    age,
    sex, 
    marital_status,
    living_situation, 
    education,
    years_education, 
    profession, 
    age_diagnose_pd, 
    deep_brain_stimulation_yn,
    level_of_care, 
    updrs_ii_total, 
    updrs_iii_levodopa_yn,
    updrs_iii_total,
    pain_scale,
    paf_sum4,
    paf_grading, 
    paf_sum_coping,
    sci_worry_total,
    sci_difficulties_total,
    moca_total,
    moca_total_corrected, 
    moca_result,
    hads_d_fear_sum, 
    hads_d_depression_sum, 
    hads_d_total, 
    hads_grading, 
    pdq_8_total, 
    pdq_8_index, 
    health_literacy_total, 
    health_literacy_index
  )

# calculalte and create new variable years of illness, year's of illness in categories
clean_FoP_spss <- clean_FoP_spss%>% 
  mutate(
    years_of_illness= age - age_diagnose_pd,
    # years_cat_illness = age_categories(years_of_illness, breakers = c(0,5,10,15,20,25))
  )

#calculate and create new variable: disease stadium
clean_FoP_spss <- clean_FoP_spss %>% 
  mutate(
    disease_stage = case_when(
      updrs_iii_total < 20 ~ "Stage 1",
      updrs_iii_total < 40 ~ "Stage 2",
      updrs_iii_total < 60 ~ "Stage 3",
      updrs_iii_total < 80 ~ "Stage 4",
      TRUE ~ "Stage 5"
    )
  )
#Nominale Variablen als diese Konvertieren
clean_FoP_spss <- clean_FoP_spss %>%
  mutate(
    marital_status = factor(marital_status, levels = c(1,2,3,4,5,6,7),labels = c("single", "married", "divorced", "widowed","in relationship", "in break up","N/A")),
    sex = factor(sex, levels = c(1,2),labels = c("masculin","feminin")),
    education = factor(education, levels = c(1,2,3,4,5,6,7), labels = c("no school leaving certificate","elemantary school","secondary school","medium maturity","vocational diploma", "high school diploma", "other")),
    paf_grading = factor(paf_grading, levels = c(1,2,3), labels = c("low FoP","moderate FoP","dysfuntional FoP")),
    hads_grading = factor(hads_grading, levels = c(0,1,2), labels = c("unobtrusive","borderline","clinical relevant"))
  )
# "dummy" Interview-Statements hinzufügen
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
patient_statements <- patient_statements[sample(1:nrow(patient_statements), 65, replace = TRUE), , drop = FALSE]

# Zusammenführen des Datensatzes mit den Statements und den ursprünglichen Daten
clean_FoP_spss <- cbind(clean_FoP_spss, patient_statements)


#App--------

# UI-Teil----
ui <- fluidPage(
  titlePanel(h1("Fear of Progression in Parkinson's Patients")),       #Überschrift
  
  #Statistische Felder mit Kennzahlen
  
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
  
  #Navigationsleiste----
  
  # Navigationsleiste
  navbarPage("",
             tabPanel("Demographic Data",           #Demographische Daten
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("var_select", "Select Variable for Plot:",                                   #Auswählen Variablen, für die Säulen-/Balkendiagramme erstellt werden sollen
                                      choices = c("sex", "disease_stage", "marital_status", 
                                                  "education", "hads_grading")),
                          sliderInput("age_range", "Select Age Range:",                                            #Darstellung durch individuell gewählte Altergruppen möglich
                                      min = min(clean_FoP_spss$age), max = max(clean_FoP_spss$age),
                                      value = c(min(clean_FoP_spss$age), max(clean_FoP_spss$age)),
                                      step = 1),
                          selectInput("dist_var", "Select Variable for Distribution Plot:",                        #Auswählen Variablen für die ein Verteilungsdiagramm erstellt werden soll
                                      choices = c("age", "age_diagnose_pd", "years_of_illness", "updrs_ii_total", 
                                                  "updrs_iii_total", "paf_sum4", "hads_d_total", "pdq_8_index"))
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
                                             choices = c("paf_sum4", "age", "years_of_illness", "hads_d_total", 
                                                         "updrs_iii_total", "pdq_8_total"),
                                             selected = c("paf_sum4", "age", "years_of_illness", "hads_d_total")),
                          helpText("Note: 'paf_sum4' should always be included in the correlation.")
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
                      sidebarLayout(                                 # Auswahl des Regressionsmodells durch den Nutzer
                        sidebarPanel(selectInput("selected_model", "Choose a Regression Model", 
                                                 choices = c("Model 1", "Model 2", "Model 3")),
                                     helpText("Note: Choose a regression model to run.")
                        ),
                        mainPanel(plotOutput("regression_plot"),                     # Regressionsgraph
                                  verbatimTextOutput("regression_results")           # Ausgabe der Regressionsparameter
                        )
                      )
             ),
             tabPanel("Interviews",                  #Interviews, aktuell Dummyvariablen
                      sidebarLayout(
                        sidebarPanel("You see a interactive Wordcloud based on the statements of the patients"),
                        mainPanel(wordcloud2Output("wordcloud", width = "100%", height = "800px"))
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
      Variable = c("Age", "Sex", "Marital Status", "Years of Education", 
                   "Age at Diagnosis", "UPDRS II Total", "UPDRS III Total"),
      N = c(sum(!is.na(filtered_data()$age)),
            sum(!is.na(filtered_data()$sex)),
            sum(!is.na(filtered_data()$marital_status)),
            sum(!is.na(filtered_data()$years_education)),
            sum(!is.na(filtered_data()$age_diagnose_pd)),
            sum(!is.na(filtered_data()$updrs_ii_total)),
            sum(!is.na(filtered_data()$updrs_iii_total))),
      Median = c(median(filtered_data()$age, na.rm = TRUE),
                 NA,                                                        #für kategoriale Variablen werden entsprechende Werte nicht berechnet
                 NA,
                 median(filtered_data()$years_education, na.rm = TRUE),
                 median(filtered_data()$age_diagnose_pd, na.rm = TRUE),
                 median(filtered_data()$updrs_ii_total, na.rm = TRUE),
                 median(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Mean = c(mean(filtered_data()$age, na.rm = TRUE),
               NA,  
               NA,
               mean(filtered_data()$years_education, na.rm = TRUE),
               mean(filtered_data()$age_diagnose_pd, na.rm = TRUE),
               mean(filtered_data()$updrs_ii_total, na.rm = TRUE),
               mean(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      SD = c(sd(filtered_data()$age, na.rm = TRUE),
             NA,  
             NA,
             sd(filtered_data()$years_education, na.rm = TRUE),
             sd(filtered_data()$age_diagnose_pd, na.rm = TRUE),
             sd(filtered_data()$updrs_ii_total, na.rm = TRUE),
             sd(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Min = c(min(filtered_data()$age, na.rm = TRUE),
              NA,  
              NA,
              min(filtered_data()$years_education, na.rm = TRUE),
              min(filtered_data()$age_diagnose_pd, na.rm = TRUE),
              min(filtered_data()$updrs_ii_total, na.rm = TRUE),
              min(filtered_data()$updrs_iii_total, na.rm = TRUE)),
      Max = c(max(filtered_data()$age, na.rm = TRUE),
              NA,  
              NA,
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
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_bar() +
        labs(title = paste("Bar Plot of", selected_var), x = selected_var, y = "Count")}
  })
  
  # Tortendiagramm
  output$pie_chart <- renderPlot({
    selected_var <- input$var_select
    
    if (selected_var %in% colnames(filtered_data())) {       #Sicherstellen, dass die ausgewählte Variable existiert
      var_data <- filtered_data()[[selected_var]]
      var_table <- table(var_data)
      var_percentage <- prop.table(var_table) * 100
      pie(var_percentage, labels = paste(names(var_percentage), round(var_percentage, 1), "%"), 
          main = paste("Pie Chart of", selected_var))}
  })
  
  #Verteilungsdiagramm
  output$dist_plot <- renderPlot({
    selected_var <- input$dist_var
    
    if (selected_var %in% colnames(filtered_data())) {     #Sicherstellen, dass die ausgewählte Variable existiert
      ggplot(filtered_data(), aes(x = .data[[selected_var]])) +
        geom_histogram(bins = 20, fill = "lightblue", color = "black", alpha = 0.7) +
        labs(title = paste("Distribution of", selected_var), x = selected_var, y = "Frequency")}
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
    
    #Heatmap mit ggplot2.    Farben ändern!
    ggplot(corr_melt, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
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
    
    #Erstellen des Scatterplots
    ggplot(regression_data, aes(x = paf_sum4, y = fitted_values)) +
      geom_point(aes(color = paf_sum4), alpha = 0.6) +
      geom_smooth(method = "lm", color = "red", se = FALSE) +
      labs(title = paste("Regression: PaF vs.", paste(selected_vars, collapse = ", ")), 
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
    
    #Textdaten für die Wordcloud extrahieren
    text_data <- clean_FoP_spss$Statement
    
    #Textdaten reinigen
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("de"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    #Erstellen einer Term-Document-Matrix
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    #Erstellen eines Datenrahmens mit den Wörtern und Häufigkeiten
    word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    #Generierung der Wordcloud
    wordcloud2(word_data, size = 0.5, color = "random-light", backgroundColor = "white")
  })
  
}
# App starten
shinyApp(ui = ui, server = server)












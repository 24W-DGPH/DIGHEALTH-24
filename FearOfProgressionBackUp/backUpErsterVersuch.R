
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

# Benötigte Pakete laden
library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(corrplot)
library(tm)
library(wordcloud)
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

str(clean_FoP_spss)

#App------

# Manuell eingegebene Inzidenz von Parkinson in Deutschland
parkinson_inzidenz_deutschland <- 112  # pro 100.000 Einwohner

# UI-Definition-----
ui <- fluidPage(
  # Überschrift
  titlePanel(h1("Fear of Progression in Parkinson's Patients", align = "left")),
  
  # Statistische Felder-----
  fluidRow(
    column(width = 4,
           div(class = "well",
               h4("Fear of Progression"),
               p(paste0(
                 percentage_over_52 <- clean_FoP_spss %>%
                   summarise( total = n(), count_over_52 = sum(paf_sum4 >52, na.rm = TRUE),
                              percentage_over_52 = (count_over_52/total)*100
                   )
               )    
               )
           )
    ),
    
    column(width = 4,
           div(class = "well",
               h4("Incidence of Parkinson's disease in Germany"),
               p(paste0(parkinson_inzidenz_deutschland, "per 100.000"))
           )
    ),
    column(width = 4,
           div(class = "well",
               h4("Average age at diagnosis"),
               p(paste0(round(mean(clean_FoP_spss$age_diagnose_pd), 1), " years"))
           )
    )
  ),
  
  # Navigationsleiste----
  navbarPage(
    title = "Navigation",
    
    # Demographische Daten
    tabPanel("Demographic Data",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable", "Choose Variable:", 
                             choices = c(
                               "Sex" = "sex", 
                               "Age" = "age", 
                               "UPDRS Score" = "updrs_iii_total",
                               "PD Stage" = "disease_stage",
                               "Years of Illness" = "years_of_illness",
                               "Marital status" = "marital_status",
                               "Education" = "education",
                               "Fear of Progression" = "paf4_sum",
                               "Stage of FoP" = "paf_grading",
                               "Distribution of Depression" = "hads_grading",
                               "Quality of Life" = "pdq_8_index"
                             )
                 )
               ),
               mainPanel(
                 fluidRow(
                   uiOutput("demographic_plots"),
                   dataTableOutput("demographic_table")
                 )
               )
             )
    ),
    
    # Korrelationen
    tabPanel(
      "Correlationen",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("correlation_vars",
                             "Choose Variables:",
                             choices = c("Age" = "age", 
                                         "Years of Illness" = "years_of_illness", 
                                         "Depression Grading" = "hads_grading",
                                         "Stage of Disease" = "disease_stage",
                                         "Quality of Life"="pdq_8_index"),
                             selected = c("age", "years_of_illness", "hads_grading", "disease_stage", "pdq_8_index")
          ),
          
          selectInput("correlation_variable", 
                      "Choose Variable for Correlation with FoP:",
                      choices = c("Age" = "age", 
                                  "Years of Illness" = "years_of_illness", 
                                  "Depression Grading" = "hads_grading",
                                  "Stage of Disease" = "disease_stage",
                                  "Quality of Life"="pdq_8_index")
          )
        ),
        
        mainPanel(
          fluidRow(
            # Korrelation Heatmap
            column(6, plotOutput("correlation_heatmap")),
            # Korrelation Scatterplot
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
            "Choose variables to integrate into Regression:",
            choices = c("age", "sex", "updrs_ii_total", "hads_d_depression","years_of_illness"),
            selected = c("age", "sex", "updrs_ii_total", "hads_d_depression","years_of_illness")
          )
        ),
        mainPanel(plotOutput("regression_plot"))
      )
    ),
    
    # Textanalyse
    tabPanel("Textanalyse", plotOutput("wordcloud"))
  ) 
  
  
  # Server-Logik-----
  server <- function(input, output, session) {
    
    # Demographische Daten: Balken-, Tortendiagramm oder Verteilungsdiagramm
    output$demographic_plots <- renderUI({
      switch(input$variable,
             "age" = plotOutput("distribution_plot_age"),                        # Diagramm für Age
             "updrs_iii_total" = plotOutput("distribution_plot_updrs"),          # Diagramm für UPDRS-Score
             "paf4_sum" = plotOutput("distribution_plot_paf4"),                  # Diagramm für PAF4-Summe
             "years_of_illness" = plotOutput("distribution_plot_years"),         # Diagramm für Jahre der Erkrankung
             fluidRow(                                                           # Falls keine der obigen Variablen ausgewählt wird
               column(6, plotlyOutput("bar_plot")),
               column(6, plotlyOutput("pie_chart"))
             )
      )
    })
    
    output$distribution_plot_age <- renderPlot({
      req(input$variable == "age")
      ggplot(clean_FoP_spss) +
        geom_histogram(aes(x = age), binwidth = 5, fill = "steelblue", alpha = 0.7) +
        geom_histogram(aes(x = age_diagnose_pd), binwidth = 5, fill = "lightblue", alpha = 0.5) +
        labs(
          title = "age distribution and age at diagnosis",
          x = "Age",
          y = "Frequency"
        ) +
        theme_minimal()
    })
    output$distribution_plot_updrs <- renderPlot({
      ggplot(clean_FoP_spss, aes(x = updrs_iii_total)) +
        geom_histogram(binwidth = 5, fill = "lightblue", alpha = 0.7) +
        labs(title = "UPDRS Score Distribution", x = "UPDRS Score", y = "Frequency") +
        theme_minimal()
    })
    
    output$distribution_plot_paf4 <- renderPlot({
      ggplot(clean_FoP_spss, aes(x = paf4_sum)) +
        geom_histogram(binwidth = 5, fill = "lightgreen", alpha = 0.7) +
        labs(title = "PAF4 Sum Distribution", x = "PAF4 Sum", y = "Frequency") +
        theme_minimal()
    })
    
    output$distribution_plot_years <- renderPlot({
      ggplot(clean_FoP_spss, aes(x = years_of_illness)) +
        geom_histogram(binwidth = 5, fill = "lightcoral", alpha = 0.7) +
        labs(title = "Years of Illness Distribution", x = "Years of Illness", y = "Frequency") +
        theme_minimal()
    })
    
    output$bar_plot <- renderPlotly({
      req(input$variable)
      var_data <- clean_FoP_spss[[input$variable]]
      
      if (is.factor(var_data) || is.character(var_data)) {
        plot_data <- as.data.frame(table(var_data))
        colnames(plot_data) <- c("Category", "Count")
        
        plot_ly(
          data  = plot_data,
          x = ~Category,
          y = ~Count,
          type = "bar",
          name = "",
          marker = list(color = "steelblue")
        ) %>%
          layout(
            title = paste("", input$variable),
            xaxis = list(title = input$variable),
            yaxis = list(title = "Frequency")
          )
      } else {
        NULL
      }
    })
    
    output$pie_chart <- renderPlotly({
      req(input$variable)
      var_data <- clean_FoP_spss[[input$variable]]
      
      if (is.factor(var_data) || is.character(var_data)) {
        plot_data <- as.data.frame(table(var_data))
        colnames(plot_data) <- c("Category", "Count")
        
        plot_ly(
          clean_FoP_spss = plot_data,
          labels = ~Category,
          values = ~Count,
          type = "pie",
          textinfo = "label+percent",
          marker = list(colors = c("lightblue", "lightyellow", "lightgreen", "pink"))
        ) %>%
          layout(
            title = paste("", input$variable)
          )
      } else {
        NULL
      }
    })
    
    
    # Tabelle für Demographische Daten
    # soll zeigen: N, %, Median, Mittelwert SD, Minimum und Maximum von Alter,
    # Geschlecht, Familienstand, Anzahl Schuljahre, Alter bei Dg-Stelleung,
    # UPDRS- II Gesamt-Wert/ III, 
    
    output$demographic_table <- renderDataTable({
      
      # Statistiken zu berechnen
      calc_stats <- function(var_data) {
        if (is.numeric(var_data)) {.                     # Für numerische Daten: Mittelwert, Median, SD, Min, Max
          stats <- data.frame(
            N = sum(!is.na(var_data)),
            Median = median(var_data, na.rm = TRUE),
            Mean = mean(var_data, na.rm = TRUE),
            SD = sd(var_data, na.rm = TRUE),
            Min = min(var_data, na.rm = TRUE),
            Max = max(var_data, na.rm = TRUE)
          )
        } else if (is.factor(var_data) || is.character(var_data)) {
          # Für kategoriale Daten: N und % pro Kategorie
          stats <- data.frame(
            N = sum(!is.na(var_data)),
            Percent = prop.table(table(var_data)) * 100
          )
        }
        return(stats)
      }
      
      # Berechne die Statistiken für jede Variable
      summary_data <- data.frame(
        Variable = c("Age", "Sex", "Marital Status", "Years of Education", 
                     "Age at Diagnosis", "UPDRS II Total", "UPDRS III Total"),
        N = c(sum(!is.na(clean_FoP_spss$age)),
              sum(!is.na(clean_FoP_spss$sex)),
              sum(!is.na(clean_FoP_spss$marital_status)),
              sum(!is.na(clean_FoP_spss$years_education)),
              sum(!is.na(clean_FoP_spss$age_diagnose_pd)),
              sum(!is.na(clean_FoP_spss$updrs_ii_total)),
              sum(!is.na(clean_FoP_spss$updrs_iii_total))),
        Median = c(median(clean_FoP_spss$age, na.rm = TRUE),
                   NA,                                                   #  kategoriale variable
                   NA,
                   median(clean_FoP_spss$education, na.rm = TRUE),
                   median(clean_FoP_spss$age_diagnose_pd, na.rm = TRUE),
                   median(clean_FoP_spss$updrs_ii_total, na.rm = TRUE),
                   median(clean_FoP_spss$updrs_iii_total, na.rm = TRUE)),
        Mean = c(mean(clean_FoP_spss$age, na.rm = TRUE),
                 NA,  
                 NA,
                 mean(clean_FoP_spss$education, na.rm = TRUE),
                 mean(clean_FoP_spss$age_diagnose_pd, na.rm = TRUE),
                 mean(clean_FoP_spss$updrs_ii_total, na.rm = TRUE),
                 mean(clean_FoP_spss$updrs_iii_total, na.rm = TRUE)),
        SD = c(sd(clean_FoP_spss$age, na.rm = TRUE),
               NA,  
               NA,
               sd(clean_FoP_spss$education, na.rm = TRUE),
               sd(clean_FoP_spss$age_diagnose_pd, na.rm = TRUE),
               sd(clean_FoP_spss$updrs_ii_total, na.rm = TRUE),
               sd(clean_FoP_spss$updrs_iii_total, na.rm = TRUE)),
        Min = c(min(clean_FoP_spss$age, na.rm = TRUE),
                NA,  
                NA,
                min(clean_FoP_spss$education, na.rm = TRUE),
                min(clean_FoP_spss$age_diagnose_pd, na.rm = TRUE),
                min(clean_FoP_spss$updrs_ii_total, na.rm = TRUE),
                min(clean_FoP_spss$updrs_iii_total, na.rm = TRUE)),
        Max = c(max(clean_FoP_spss$age, na.rm = TRUE),
                NA,  
                NA,
                max(clean_FoP_spss$education, na.rm = TRUE),
                max(clean_FoP_spss$age_diagnose_pd, na.rm = TRUE),
                max(clean_FoP_spss$updrs_ii_total, na.rm = TRUE),
                max(clean_FoP_spss$updrs_iii_total, na.rm = TRUE))
      )
      
      # Zeige die Tabelle
      datatable(summary_data, options = list(pageLength = 5), rownames = FALSE)
    })
    
    
    # Korrelationen: Heatmap und Scatterplot
    
    output$correlation_heatmap <- renderPlot({
      selected_vars <- c("paf_sum4", input$correlation_vars)           #PAF ist immer eingefügt
      correlation_data <- clean_FoP_spss[,selected_vars,drop = FALSE]  #ausgewählte Variablen
      corr_matrix <- cor(correlation_data, use="complete.obs")
      corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black")
    })
    
    output$scatterplot_correlation <- renderPlot({
      req(input$correlation_variable)  
      y_variable <- input$correlation_variable
      ggplot(clean_FoP_spss, aes(x = "paf_sum4", y = y_variable)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE, color = "lightblue") +
        labs(
          title = "Correlations of Fear of Progression",
          x = "Fear of Progression",
          y = y_variable
        )
    })
    
    # Lineare Regression für Fear of Progression und Visualisierung
    output$regression_plot <- renderPlot({
      req(input$regression_vars)  
      
      # Erstellen der Formel für die lineare Regression
      formula <- as.formula(paste("paf_sum4 ~", paste(input$regression_vars, collapse = " + ")))
      
      # Lineare Regression durchführen
      fit <- lm(formula, data =  clean_FoP_spss)
      
      print(summary(fit))
      
      # Visualisierung der Regression
      ggplot(clean_FoP_spss, aes_string(x = input$regression_vars[1], y = "paf_sum4")) +
        geom_point() +                                                                      # Streudiagramm für die Daten
        geom_smooth(method = "lm", se = TRUE, color = "red") +                              # Regressionslinie hinzufügen
        labs(
          title = paste("Lineare Regression: Fear of Progression ~", paste(input$regression_vars, collapse = " + ")),
          x = input$regression_vars[1],
          y = "Fear of Progression"
        )
    })
    
    # Textanalyse: Wordcloud
    output$wordcloud <- renderPlot({
      wordcloud(clean_FoP_spss$Text, max.words = 100, colors = c("lightblue","lightgreen","pink","lightyellow"))
    })
  } 
)
# App starten
shinyApp(ui, server) 


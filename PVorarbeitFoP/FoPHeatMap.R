
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
  dplyr.          #install
)

#import dataset ------
FoP_spss <- read_sav("/Applications/Progredienzangst_SPSS_09.05.2019_final Kopie 2.sav")


#control
#View(FoP_spss)


#Test exploratory analysis

#nrow(FoP_spss)
#ncol(FoP_spss)
#summary(FoP_spss)
#tabyl(FoP_spss)
#skim(FoP_spss)

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

#view(clean_FoP_spss)    #proof
#names(clean_FoP_spss)   #proof


#first try graphic
ggplot(data=clean_FoP_spss, mapping= aes(x= age_diagnose_pd))+geom_histogram()     

#testing
#clean_FoP_spss %>% 
#  tabyl(profession)

#clean_FoP_spss %>% 
#  tabyl(paf_grading)

# calculalte and create new variable years of illness, year's of illness in categories
clean_FoP_spss <- clean_FoP_spss%>% 
  mutate(
    years_of_illness= age - age_diagnose_pd,
    years_cat_illness = age_categories(years_of_illness, breakers = c(0,5,10,15,20,25)))



#test  
#clean_FoP_spss %>% tabyl(years_cat_illness)



#ShinyApp running-------
data <- clean_FoP_spss

# User Interface (UI)
ui <- fluidPage(
  titlePanel("Interaktive Heatmap von Korrelationen"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "selected_vars",
        "Wähle Variablen für die Heatmap:",
        choices = colnames(data),
        selected = colnames(data)  # Standardmäßig alle ausgewählt
      )
    ),
    
    mainPanel(
      plotOutput("heatmap")
    )
  )
)

# Server.  
server <- function(input, output) {# variablen müssen anders charakteriisiert werden ->'x' must be numeric; wie kann ich nur bestimmte variablen auswählen??
  
  output$heatmap <- renderPlot({
    # Überprüfe, ob mindestens zwei Variablen ausgewählt sind
    if (length(input$selected_vars) < 2) {
      return(NULL)
    }
    
    # Datensatz filtern, um nur ausgewählte Variablen zu verwenden
    selected_data <- data[, input$selected_vars, drop = FALSE]
    
    # Korrelationen berechnen
    cor_matrix <- cor(selected_data)
    
    # Korrelationen in ein Format für ggplot umformen
    melted_cor <- melt(cor_matrix)
    
    # Heatmap erstellen
    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "red", mid = "white", high = "blue",
        midpoint = 0, limit = c(-1, 1),
        name = "Korrelationswert"
      ) +
      theme_minimal() +
      labs(title = "Heatmap der Korrelationen", x = "", y = "") +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 12)
      )
  })
}

# Shiny App starten
shinyApp(ui = ui, server = server)


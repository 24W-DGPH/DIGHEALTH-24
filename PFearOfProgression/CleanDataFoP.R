
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
#FoP_spss <- read_sav("/Applications/Progredienzangst_SPSS_09.05.2019_final Kopie 2.sav")

data <- read_sav("./Progredienzangst.sav")
FoP_spss <- data

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


#Synonyme für Variablen erstellen-----

#Anzeige der Variablen ändern für Demographic Balken
variable_mapping1 <- data.frame(
  variable = c ("sex", "disease_stage", "marital_status", 
                "education", "hads_grading"),
  synonym = c ("Sex", "Diesease Stage", "Marital Status", "Education", "Stage of Depression"),
  stringsAsFactors = FALSE
)

#Anzeige der Variablen ändern für Demographic Verteilung
variable_mapping2 <- data.frame(
  variable = c ("age", "age_diagnose_pd", "years_of_illness", "updrs_ii_total", 
                "updrs_iii_total", "paf_sum4", "hads_d_total", "pdq_8_index"),
  synonym = c ("Age", "Age at Diagnosis", "Years of Illness", "UPDRSII", "UPDRSIII","FoP","Depression (HADS-D)","Quality of Life"),
  stringsAsFactors = FALSE
)

#Anzeige der Variablen ändern für Correlation
variable_mapping3 <- data.frame(
  variable = c ("paf_sum4", "age", "years_of_illness", "hads_d_total", 
                "updrs_iii_total", "pdq_8_total"),
  synonym = c ("FoP", "Age", "Years of Illness", "Depression (HADS-D)", "UPDRSIII","Quality of Life"),
  stringsAsFactors = FALSE
)

#Anzeige der Variablen ändern für Regression
variable_mapping4 <- data.frame(
  variable = c("age", "age_diagnose_pd", "education", "disease_stage", "years_of_illness", "hads_d_total"),
  synonym = c("Age", "Age at Diagnosis", "Education", "Disease Stage", "Years of Illness", "Depression (HADS-D)"),
  stringsAsFactors= FALSE
)


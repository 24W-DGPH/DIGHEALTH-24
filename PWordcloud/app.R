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

additional_stopwords <- c("dass", "und", "oder", "ist", "es", "nicht", "ich", "wie", "das", 
                          "die", "der", "den", "ein", "eine", "sich", "kann", "schon", 
                          "noch", "wird", "muss", "meine", "mir", "für")

# UI
ui <- navbarPage(
  title = "Analyse-Dashboard",  # Titel der Navbar
  tabPanel("Wordcloud",  # Tab für die Wordcloud
           fluidPage(
             titlePanel(h3("Wordcloud", align = "center")),  # Haupttitel
             #h4("Diese Wordcloud zeigt die wichtigsten Begriffe aus den Patientenstatements.", align = "center"),  # Erklärungstext
             wordcloud2Output("wordcloud", width = "100%", height = "1000px")  # Große Wordcloud
           )
  )
)

# Server
server <- function(input, output, session) {
  output$wordcloud <- renderWordcloud2({
    # Textdaten für die Wordcloud extrahieren
    text_data <- patient_statements$Statement
    
    # Textdaten reinigen
    corpus <- Corpus(VectorSource(text_data))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("de"))
    corpus <- tm_map(corpus, removeWords, additional_stopwords)
    corpus <- tm_map(corpus, stripWhitespace)
    
    # Erstellen einer Term-Document-Matrix
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    # Erstellen eines Datenrahmens mit den Wörtern und Häufigkeiten
    word_data <- data.frame(word = names(word_freqs), freq = word_freqs)
    
    # Generierung der Wordcloud
    wordcloud2(word_data, size = 1, color = "random-light", backgroundColor = "white")
  })
}

# App starten
shinyApp(ui = ui, server = server)


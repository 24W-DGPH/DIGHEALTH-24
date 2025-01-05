#

library(shiny)

# Define UI application that draws a histogram
ui <-  fluidPage(
   title = "Our first Site",
   includeHTML("testR.html")
)

#define server logic required to draw a histogram
server <- function(input, output)

# Run the application
  shinyApp(ui=ui, server=server)
  
  
  


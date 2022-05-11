library(shiny)
library(readr)
#rezeptpflicht <- read_csv("data/Rezeptpflicht/rezeptpflicht.csv")
rezeptpflicht <- readRDS("~/data/Rezeptpflicht/rezeptpflicht.rds")
ui <- fluidPage(
  selectizeInput("WS", "Wirkstoff",choices = NULL),
  textOutput("Rstatus")
  
  )


server <- function(input, output, session) {
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )

  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezeptpflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })
}


shinyApp(ui = ui, server = server)

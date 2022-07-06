
library(shiny)

ui <- fluidPage(
  createRezeptursammlungUI("jun_and_int"),
  uiOutput("selectizeInput01"),
  foundRezepturenButtonUI("button"),
)

server <- function(input, output, session) {
  rz <- createRezeptursammlungServer("jun_and_int")
  # rezeptursammlung <- rz$rezeptursammlung()   # browser()
  output$selectizeInput01 <- renderUI({
    
    selectizeInput("Substanz", "Zusammensetzung der Rezeptur",choices = rz$rezeptursammlung()$V2, multiple = TRUE,
                   options = list(placeholder = "wähle Substanzen aus"))
    
    
  })
  observeEvent(input$Substanz,{
    foundRezepturenButtonServer("button",input$Substanz, rz$rezeptursammlung(), rz$datapath())
  })
 
}

shinyApp(ui, server)



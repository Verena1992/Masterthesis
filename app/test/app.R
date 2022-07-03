library(shiny)


#taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
ui <- fluidPage(
  tags$h3("JunsDSdsi"),
  selectizeInput(
    "foo", label = NULL, choices = NULL,selected = NULL,multiple = TRUE,
    options = list(create = TRUE, placeholder = "select a state name" )
  )
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "foo",selected = NULL, choices = state.name)
#  updateSelectizeInput(server = TRUE)

  
  
  
  }
  
  
  


shinyApp(ui, server)
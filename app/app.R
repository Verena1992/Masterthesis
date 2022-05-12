library(shiny)
library(readr)
library(vroom)
library(dplyr)

rezeptpflicht <- readRDS("~/data/Rezeptpflicht/rezeptpflicht.rds")

ui_Rezeptpflichtcheck <- tabPanel("Rezeptpflichtcheck", 
                                  selectizeInput("WS", "Wirkstoff",choices = NULL),
                                  textOutput("Rstatus"))


ui_Suppositorien <- tabPanel("Suppositorien",
                            fileInput("Verdrängungsfaktoren", "Choose CSV File", accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                            ),
                            tableOutput("preview1"),
                            selectizeInput("WS_S", "Substanz",choices = NULL),
                            textOutput("Verdrängungsfaktor"))

ui <- navbarPage("My Application", ui_Rezeptpflichtcheck, ui_Suppositorien,
                 tabPanel("Component 3")
)



#ui <- fluidPage(
#  selectizeInput("WS", "Wirkstoff",choices = NULL),
#  textOutput("Rstatus")
  
#  )


server <- function(input, output, session) {
# Upload Verdrängungsfaktoren-----------------------------------------------------
  
  data_Verdrän <- reactive({
    req(input$Verdrängungsfaktoren)#to make sure code waits until the first file is uploaded
    #inFile <- input$file1
    #if (is.null(inFile))
    #  return(NULL)
    
    #read.csv(inFile$datapath, header = T)
    dataSet <- vroom::vroom(input$Verdrängungsfaktoren$datapath, delim = "\t")
    vars <- colnames(dataSet)
    
    dataSet
    })
  
  observe({
    dataSet <- data_Verdrän()
    updateSelectizeInput(session, "WS_S", choices = dataSet$Wirkstoff)
  })
  
  
 # output$preview1 <- renderTable(head(data_Verdrän()))
  
  
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )

  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezepflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })

  

}


shinyApp(ui = ui, server = server)

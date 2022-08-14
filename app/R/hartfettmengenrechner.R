#27/07/2022



# inputs: Verdrängungsfaktoren
# outputs: optional new added Verdrängungsfaktoren
#load_libraries()
library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)
library(purrr)
library(shinyBS)
library(pdftools)
#UI-----------------------------------------------------

hartfettmengenrechnerUI <- function(id, fileuploaded) {
  tagList(
    
    wellPanel(tags$h4(id), 
              conditionalPanel(fileuploaded,
                               
                               selectizeInput(NS(id,"WS_S"), "Substanz",choices = NULL),
                               
                              # conditionalPanel(condition = "input.WS_S == 'Substanz nicht in Liste vorhanden'",
                              #                  tags$h4(htmlOutput("sub")) ),
                               htmlOutput("vf"),
                               tags$hr()),
              conditionalPanel(fileuploaded, 
                               numericInput(NS(id,"Vf"), "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01)),
              numericInput(NS(id,"Menge_Substanz"), "Menge(g) pro Zäpfchen", value = 0.7, min = 0, max = 1, step = 0.01), 
              actionButton(NS(id,"weitere_Substanz"), "weitere Substanz hinzufügen") 
    )
    
  )
  
}







#Server-------------------------------------------------


hartfettmengenrechnerServer <- function(id, data_Verdrän) {
  moduleServer(id, function(input, output, session) {
  
    # observe({
    #   #req(rz$datapath())
    #   dataSet <- data_Verdrän()
    #   updateSelectizeInput(session, "WS_S", choices = c(dataSet$Wirkstoff, "Substanz nicht in Liste vorhanden"))
    # })
    
    observeEvent(data_Verdrän(),{
      dataSet <- data_Verdrän()
      #browser()
      updateSelectizeInput(session, "WS_S", choices = c(dataSet$Wirkstoff, "Substanz nicht in Liste vorhanden"))
    })
    
  })
}




#Test module:

# hartfettmengenrechnerApp <- function() {
#   ui <- fluidPage(
#     createVerdrängungsfaktorenUI("nrf_and_int"),
#     hartfettmengenrechnerUI("Substanz1", "output.fileUploaded")
#   )
#   
#   
#   
#   server <- function(input, output, session) {
#     #datapath<- c("interne_Rezeptursammlung_3.zip")
#     
#     #data_Verdrän <- reactive({
#       #if interne Sammlung is uploaded and nrf merge both
#       data_Verdrän <- createVerdrängungsfaktorenServer("nrf_and_int")
#    #   browser()
#   #  })
#     
#     output$fileUploaded <- reactive({
#       dataSet <- data_Verdrän()
#       return(!is.null(dataSet))
#     })
#     outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
#     
#       
#       
#     
#       hartfettmengenrechnerServer("Substanz1", data_Verdrän)
# 
#   }
#   shinyApp(ui, server)
# }
# 
# hartfettmengenrechnerApp()

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

#taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")


#UI-----------------------------------------------------

erstattungscheckUI <- function(id, Bestandteile = NULL) {
  tagList(
    selectizeInput(NS(id,"zusammensetzung_arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                   options = list(create = TRUE,placeholder = "wähle Substanzen aus"))
   # uiOutput(NS(id,"selectizeInput01"))
  )

}




#Server-------------------------------------------------
erstattungscheckServer <- function(id, taxe_eko, Bestandteile = NULL) {
  moduleServer(id, function(input, output, session) {
    
    
    
  #  ns <- session$ns
    
  #  output$selectizeInput01 <- renderUI({
      
     
  #  })
    
    
    updateSelectizeInput(session, inputId = 'zusammensetzung_arzneitaxe', choices = taxe_eko$wirkstoffe_arzneitaxe, selected = Bestandteile, server = TRUE)
    
    
  })
}

#Test module:
# 
# ErstattungscheckApp <- function() {
#   ui <- fluidPage(
#    
#     erstattungscheckUI("ec")
#   )
#   
#   server <- function(input, output, session) {
#     #Bestandteile <- c("igendepas")
#     Bestandteile <- c("Atropinsulfat", "Natriumchlorid")
#     taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
#     erstattungscheckServer("ec", taxe_eko, Bestandteile)
#   }
#   
#   shinyApp(ui, server)
# }
# # 
# ErstattungscheckApp()
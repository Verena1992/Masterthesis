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


kompatibilitätscheckUI <- function(id) {
  #ns <- NS(id)
  tagList(
    selectizeInput(NS(id,"Salbengrundlage"), "wähle eine Salbengrundlage",choices = NULL, multiple = TRUE,
                   options = list(create = FALSE,placeholder = "Salbengrundlage")),
    selectizeInput(NS(id,"Wirksubstanz"), "wähle Wirksubstanzen oder Adjuvantien aus",choices = NULL, multiple = TRUE,
                    options = list(create = FALSE,placeholder = "Salbengrundlage")),
   # Substanzauswahl()
  )
}

kompatibilitätscheckServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      physikalische.Stabilität <- read.delim("./data/Salbenfibel/physikalische Stabilität.txt", header=FALSE)
      #Salbengrundlage <- c("Ultrasicc", "Ultraphil", "Ultrabas", "Ultralip")
      updateSelectizeInput(session, inputId = 'Salbengrundlage', choices = physikalische.Stabilität$V1, selected = NULL, server = TRUE)
      updateSelectizeInput(session, inputId = 'Wirksubstanz', choices = physikalische.Stabilität$V2, selected = NULL, server = TRUE)
    }, 
    
    
  )
}

#Test module:

# KombatibilitätscheckApp <- function() {
# ui <- fluidPage(
#   kompatibilitätscheckUI("Salbenfibel")
# )
# 
# server <- function(input, output, session) {
#   kompatibilitätscheckServer("Salbenfibel")
# 
# }
# 
# shinyApp(ui, server)}
# KombatibilitätscheckApp()

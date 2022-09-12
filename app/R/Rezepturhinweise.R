
#01/07/2022 takes user textinput about "Rezepturhinweise" and returns a dataframe with the new Information  
library(purrr)
library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
#library(shinyFiles)
library(shinyjs)
library(purrr)
library(shinyBS)
library(pdftools)
#load_libraries()




#functions----------------------------------------------

textAreaInput01 <- function(id,vars) {
  #to render textinputUI
  textAreaInput(id, vars, width = "100%")
}

#UI-----------------------------------------------------
rezepturhinweiseUI <- function(id) {
  vars <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung",  "Haltbarkeit", "Lagerung", "Anwendung")
  ns <- NS(id)
  #ns(vars) = id-Titel, id-Herstellungshinweise...
  #map() calls textAreaInput01() once for each string stored in vars. It returns a list of textAreas
  sliders <- map2(ns(vars),vars, textAreaInput01)
  
    sliders
}

#Server-------------------------------------------------
rezepturhinweiseServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # The user's data, parsed into a data frame
    Herstellungshinweise <- c(
      input$Titel, input$Herstellungshinweise, input$Quelle, input$Dosierung, 
      input$Haltbarkeit, input$Lagerung, input$Anwendung)
    
    titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung", 
               "Haltbarkeit", "Lagerung", "Anwendung")
    
    new_Rezeptur <- data.frame(Herstellungshinweise)
    new_Rezeptur <- t(new_Rezeptur)
    colnames(new_Rezeptur) <- titel
    
    # Return the reactive that yields the data frame
    return(new_Rezeptur)
  })
}


# Test module:-------------------------------------------------
 # 
 # histogramApp <- function() {
 #   ui <- fluidPage(
 #     rezepturhinweiseUI("sliders"),
 #     tableOutput("new_Rezeptur")
 #   )
 #   server <- function(input, output, session) {
 #     output$new_Rezeptur <- renderTable({
 #       t(rezepturhinweiseServer("sliders"))
 #     }) 
 #     
 #   }
 #   shinyApp(ui, server)  
 # }
 # 
 # histogramApp()
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
library(DT)
#---------------------------------------------------------------------------------------------------
bedenklichStUI <- function(id) {
  #ns <- NS(id)
  tagList(
    DT::dataTableOutput(NS(id,"mytable"))
  )
}

bedenklichStServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      bedenkliche_St <- read.delim("./data/bedenkliche_Substanzen/bedenkliche_St.txt")
      
      output$mytable = DT::renderDataTable({
        bedenkliche_St
      })
      
    }
  )
}


#Test module:

bedenklichStApp <- function() {
ui <- fluidPage(
  bedenklichStUI("arzneimittelkommission"),
)

server <- function(input, output, session) {

  
  bedenklichStServer("arzneimittelkommission")



}

shinyApp(ui, server)}
bedenklichStApp()
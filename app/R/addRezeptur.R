#11/07/2022

# inputs:  taxe_eko, optional(datapath from uploaded interner Ordner)
# outputs: substanzen and input jump_2_Herstellungshinweise (until now only substanzen from ui are used)


# functions: Substanzauswahl, big_yellow_button


library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
library(shinyjs)
library(purrr)
library(shinyBS)
library(pdftools)




#UI-----------------------------------------------------

addRezepturUI <- function(id) {
  
  vars <- rep(1:5)
  Rezepturzusammensetzung <- map(NS(id, as.character(vars)), Substanzauswahl)
  
  tagList(
   Rezepturzusammensetzung,
   big_yellow_button(NS(id, "jump_2_Herstellungshinweise"), "weiter zu Herstellungshinweise")
  )
}

#Server-------------------------------------------------

addRezepturServer <- function(id, taxe_eko) {
  moduleServer(id, function(input, output, session) {
    
    bedenkliche_St <- read.delim("./data/bedenkliche_Substanzen/bedenkliche_St.txt")
    
    
    #Internal functions,  function needs to use input, output, or session it may make sense 
    #for the function to live inside the server function
    Substanzauswahl_server <- function(id){
      updateSelectizeInput(session, id, choices = c(taxe_eko$wirkstoffe_arzneitaxe,bedenkliche_St$Stoffe) , server = TRUE)
    }
    
    vars <- rep(1:5)
    Rezepturzusammensetzung_server <- map(as.character(vars), Substanzauswahl_server)
    
     new_Rezeptur <- reactive({
       Substanzen <- c(input[["1"]],input[["2"]], input[["3"]], input[["4"]], input[["5"]])
       Substanzen
     })   
     
     
     bedenkliche_Substanz <- reactive({
       bedenkliche_Substanz <- intersect(c(input[["1"]],input[["2"]], input[["3"]], input[["4"]], input[["5"]]), bedenkliche_St$Stoffe)
       bedenkliche_Substanz
     })
     
     observe({
        req(bedenkliche_Substanz())
        shinyalert(title = "Achtung!! die Rezeptur enthält eine bedenkliche Substanz ", type = "error")
     })
     
     list(
       Substanzen = reactive(new_Rezeptur()),
       jump_to_Herstellungshinweise = reactive(input$jump_2_Herstellungshinweise)
     )
     
  })
}








#Test module:

# addRezepturApp <- function() {
#   ui <- fluidPage(
# 
#     addRezepturUI("Zusammensetzung"),
#     tableOutput("text2")
#   )
# 
#   server <- function(input, output, session) {
#     taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
#     new_Rezeptur <- addRezepturServer("Zusammensetzung", taxe_eko)
#     output$text2 <- renderTable(new_Rezeptur())
#   }
# 
#   shinyApp(ui, server)
# }
# 
# addRezepturApp()

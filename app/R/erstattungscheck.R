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

erstattungscheckUI <- function(id) {
  tagList(
    selectizeInput(NS(id,"zusammensetzung_arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                   options = list(create = TRUE,placeholder = "wähle Substanzen aus")), 
    tableOutput(NS(id,"länge"))
   # uiOutput(NS(id,"selectizeInput01"))
  )

}




#Server-------------------------------------------------
erstattungscheckServer <- function(id, taxe_eko, Bestandteile) {
  moduleServer(id, function(input, output, session) {
    
    
    updateSelectizeInput(session, inputId = 'zusammensetzung_arzneitaxe', choices = taxe_eko$wirkstoffe_arzneitaxe, selected = Bestandteile, server = TRUE)
  
    sub_taxe <- reactive({
        sub_taxe <- taxe_eko[taxe_eko$wirkstoffe_arzneitaxe %in% input$zusammensetzung_arzneitaxe,]
        sub_taxe
    })
    
    box <- reactiveVal("grün")
    
   observe({
     #browser()
     #req(Bestandteile)
     req(input$zusammensetzung_arzneitaxe)
     #Box <- c()
     for (i in input$zusammensetzung_arzneitaxe){
       
    print(i)
    # Box <- "grün"
    # numlines <- which(taxe_eko$wirkstoffe_arzneitaxe == i)
     #browser()
     #Box <- append(Box,taxe_eko$box[numlines])
     
     #if(all(x < 0))
    #if(all(Box != "grün")){
     #  shinyalert(title = "Bewilligung des chef- und kontrollärztlichen Dienstes nötig! Höchstmenge überschritten", type = "warning")
     
   # }
     }
   })
    
    
    output$länge <- renderTable({
      sub_taxe()
  #    browser()
    })
  })
}

#Test module:

ErstattungscheckApp <- function() {
  ui <- fluidPage(

    erstattungscheckUI("ec")
  )

  server <- function(input, output, session) {
    #Bestandteile <- c("igendepas")
    Bestandteile <- c("Atropinsulfat", "Natriumchlorid")
    taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
    erstattungscheckServer("ec", taxe_eko, Bestandteile)
  }

  shinyApp(ui, server)
}
#
ErstattungscheckApp()
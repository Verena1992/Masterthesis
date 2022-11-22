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

erstattungscheckUI <- function(id) {
  tagList(
    actionBttn(NS(id, "reset"), "Reset"),
    tags$hr(),
    selectizeInput(NS(id,"zusammensetzung_arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                   options = list(create = FALSE,placeholder = "wähle Substanzen aus")), 
    tableOutput(NS(id,"länge"))
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
    
    element_not_green <- reactive({
      element_not_green <- sub_taxe()[which(sub_taxe()$box != "grün"),]$wirkstoffe_arzneitaxe
    })
    
    element_not_found <- reactive({
      req(Bestandteile)
      if (length(input$zusammensetzung_arzneitaxe) != length(Bestandteile)){
        #the command setdiff(list.a, list.b) finds the non-overlapping elements only 
        #if these elements are contained in the object that is used as the first argument
        element_not_found <- setdiff(Bestandteile, input$zusammensetzung_arzneitaxe)
      }
    })
    

    output$länge <- renderTable({
      sub_taxe()
    })
    
    observeEvent(input$reset,{
      reset("zusammensetzung_arzneitaxe")
    })
    
    list(
      element_not_found = reactive(element_not_found()), 
      not_green = reactive(element_not_green())
    )
 
  })
}

#Test module:

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
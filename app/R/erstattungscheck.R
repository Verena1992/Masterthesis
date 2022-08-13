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
#??? wenn bei rezeptursammlung mit einer Substanz gesucht wird auf button geklickt und dann nochmals mit die selbe Substanz eingegeben bleibt 
# info über geklickten button gespeichert

#UI-----------------------------------------------------

erstattungscheckUI <- function(id) {
  tagList(
    actionBttn(NS(id, "reset"), "Reset"),
    tags$hr(),
    selectizeInput(NS(id,"zusammensetzung_arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                   options = list(create = TRUE,placeholder = "wähle Substanzen aus")), 
    tableOutput(NS(id,"länge")),
    
   # textOutput(NS(id, "enf"))

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
    
  #element_not_green <- react
    
   # element_not_found <- reactive({
   #   req(Bestandteile)
   #   req(input$zusammensetzung_arzneitaxe)
   #   if (length(input$zusammensetzung_arzneitaxe) != length(Bestandteile)){
   #     #the command setdiff(list.a, list.b) finds the non-overlapping elements only 
   #     #if these elements are contained in the object that is used as the first argument
   #     element_not_found <- setdiff(Bestandteile, input$zusammensetzung_arzneitaxe)
   #     
   #   }
     
     element_not_found <- reactive({
       req(Bestandteile)
       req(input$zusammensetzung_arzneitaxe)
       if (length(input$zusammensetzung_arzneitaxe) != length(Bestandteile)){
         #the command setdiff(list.a, list.b) finds the non-overlapping elements only 
         #if these elements are contained in the object that is used as the first argument
         element_not_found <- setdiff(Bestandteile, input$zusammensetzung_arzneitaxe)
         
       }
     
     
    # observeEvent(input$reset,{
    #   element_not_found <- c()
    # })
     
     
   #  for (i in input$zusammensetzung_arzneitaxe){
       
   # print(i)
    # Box <- "grün"
    # numlines <- which(taxe_eko$wirkstoffe_arzneitaxe == i)
     #browser()
     #Box <- append(Box,taxe_eko$box[numlines])
     
     #if(all(x < 0))
    #if(all(Box != "grün")){
     #  shinyalert(title = "Bewilligung des chef- und kontrollärztlichen Dienstes nötig! Höchstmenge überschritten", type = "warning")
     
   # }
    # }
   })
    
   # output$enf <- renderText({
   #   element_not_found()
   # })
   
    
    output$länge <- renderTable({
      sub_taxe()
    })
    
    
    observeEvent(input$reset,{
      print(input$reset)
      reset("zusammensetzung_arzneitaxe")
     # Bestandteile <- NULL
    })
    
    #btn <- reactive(input$reset)
    #return rezeptursammlung_dataset and datapath
    list(
      element_not_found = reactive(element_not_found()), 
      input = reactive(input$zusammensetzung_arzneitaxe)
      # = reactive(btn()))
    #  rezeptursammlung = reactive(rezeptursammlung()),

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
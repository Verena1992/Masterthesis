#22/07/2022

#1. (optional) unzip, read in of private Verdränungsfaktoren
#2. (optional) upload NRF-Verdrängungsfaktoren
#3. merge optional uploaded NRF-Verdrängungsfaktoren with optional uploaded private NRF-Verdrängungsfaktoren.
#4. outputs Rezeptursammlungdataset and datapath of uploaded file

# inputs: (optional), (optional) datapath interner zip ordner containing private NRF-Verdrängungsfaktoren
# outputs: rezeptursammlungdataset and optional(datapath from uploaded interner Ordner)


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

createVerdrängungsfaktorenUI <- function(id) {
  tagList(
    fileInput(NS(id, "Verdrängungsfaktoren"), 
              label = h4("NRF-Verdrängungsfaktoren",
                         tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                         bsButton(NS(id,"q1"), label = "", icon = icon("question"), style = "info", size = "extra-small")
              )),
    bsPopover(NS(id,"q1"), title = "NRF - Verdrängungsfaktoren",
              content = paste0("lade die heruntergeladene AnlageF aus dem NRF (als pdf) hoch, um Sie Ihrer internen Sammlung hinzuzufügen."
              ),
              placement = "right", 
              trigger = "focus")
    
  )
}

#Server-------------------------------------------------

createVerdrängungsfaktorenServer <- function(id, datapath=NULL) {
  moduleServer(id, function(input, output, session) {
    
    #readin interne Verdrängungsfaktoren
    if (!is.null(datapath)){
    interne_Verdrängungsfaktoren <- reactive({
      #print(datapath)
      req(datapath)
      dataSet <- zip2dataSet(datapath, filenr = 3)
      dataSet
    })}
    
    
    #readin NRF Verdrängungsfaktoren 
    NRF_Verdrängungsfaktoren <- reactive({
      req(input$Verdrängungsfaktoren)
      dataSet <- parse_NRF_verdrängungsfaktoren(input$Verdrängungsfaktoren$datapath)
      dataSet
    })
    
    
    
    #merge NRF with interne if uploaded
    verdrängungsfaktoren <- reactive({
      if (!is.null(input$Verdrängungsfaktoren) & !is.null(datapath)) {
        dataSet <- rbind(interne_Verdrängungsfaktoren(), NRF_Verdrängungsfaktoren())
      } else if (!is.null(input$Verdrängungsfaktoren)){
        dataSet <- NRF_Verdrängungsfaktoren()
      } else if (!is.null(datapath)){
        dataSet <- interne_Verdrängungsfaktoren()
      } else {
        NULL
      }
    })

    
    
    verdrängungsfaktoren

    
  })
}


#Test module:

# VerdrängungsfaktorenApp <- function() {
#   ui <- fluidPage(
#     createVerdrängungsfaktorenUI("nrf_and_int"),
#     tableOutput("table"),
#   )
# 
#   server <- function(input, output, session) {
#     datapath<- c("interne_Rezeptursammlung_3.zip")
#     
#       if (exists("datapath")){
#         verdrängungsfaktoren <- createVerdrängungsfaktorenServer("nrf_and_int", datapath)
#       } else {
#         #browser()
#         verdrängungsfaktoren <- createVerdrängungsfaktorenServer("nrf_and_int")}
# 
#   output$table <- renderTable(
#     verdrängungsfaktoren())
#   }
# 
#   shinyApp(ui, server)
# }
# 
#   VerdrängungsfaktorenApp()





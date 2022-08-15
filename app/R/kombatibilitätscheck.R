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
    tags$h3("physikalische Stabilität"),
   
    tableOutput(NS(id,"table_ph")), 
    helpText("Quelle: F. Kalb, Mischbarkeit von Salbengrundlagen mit verschiedenen Wirkstoffen, Diplomarbeit Universität Wien 2005"),
    tags$hr(),
    tags$h3("chemische Stabilität"),
    tableOutput(NS(id,"table_ch")), 
    helpText("Quelle: A. Tesar, Chemische Stabilität von ausgewählten Wirkstoffen in Salbengrundlagen, Universität Wien 2006")
    
   
  )
}

kompatibilitätscheckServer <- function(id, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      physikalische.Stabilität <- read.delim("./data/Salbenfibel/physikalische Stabilität.txt", header=TRUE)
      chemische.Stabilität <- read.delim("./data/Salbenfibel/chemische Stabilität.txt", header=TRUE)
      #Salbengrundlage <- c("Ultrasicc", "Ultraphil", "Ultrabas", "Ultralip")
      updateSelectizeInput(session, inputId = 'Salbengrundlage', choices = c(physikalische.Stabilität$Salbengrundlage,chemische.Stabilität$Salbengrundlage), selected = Bestandteile, server = TRUE)
      updateSelectizeInput(session, inputId = 'Wirksubstanz', choices = c(physikalische.Stabilität$Substanz, chemische.Stabilität$Substanz), selected = Bestandteile, server = TRUE)
    
    
    sub_physika_Stabil <- reactive({
      sub_physika_Stabil <- physikalische.Stabilität[physikalische.Stabilität$Salbengrundlage %in% input$Salbengrundlage,]
      sub_sub_physika_Stabil <- sub_physika_Stabil[sub_physika_Stabil$Substanz  %in% input$Wirksubstanz,]
    })
    
    sub_chemi_Stabil <- reactive({
      sub_chemi_Stabil <- chemische.Stabilität[chemische.Stabilität$Salbengrundlage %in% input$Salbengrundlage,]
      sub_sub_chemi_Stabil <- sub_chemi_Stabil[sub_chemi_Stabil$Substanz  %in% input$Wirksubstanz,]
    })
    
    output$table_ph <- renderTable({
      sub_physika_Stabil()
    })
    
    output$table_ch <- renderTable({
      sub_chemi_Stabil()
    })
    
    element_not_found <- reactive({
      req(Bestandteile)
      req(input$Salbengrundlage | input$Wirksubstanz)
      browser()
      if (sum(length(input$Salbengrundlage),length(input$Wirksubstanz)) != length(Bestandteile)){
        #the command setdiff(list.a, list.b) finds the non-overlapping elements only 
        #if these elements are contained in the object that is used as the first argument
      #  browser()
        element_not_found <- setdiff(Bestandteile, input$zusammensetzung_arzneitaxe)
      }
      })
    
    #list(
     # element_not_found = reactive(element_not_found()), 
      #not_green = reactive(element_not_green())
      # = reactive(btn()))
      #  rezeptursammlung = reactive(rezeptursammlung()),
      
    #)
    
    })
}

#Test module:

KombatibilitätscheckApp <- function() {
ui <- fluidPage(
  kompatibilitätscheckUI("Salbenfibel")
)

server <- function(input, output, session) {
  Bestandteile <- c("Zinkoxid", "Ultrasicc (R)", "blabla")
  kompatibilitätscheckServer("Salbenfibel", Bestandteile)


}

shinyApp(ui, server)}
KombatibilitätscheckApp()

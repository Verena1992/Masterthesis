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
kompatibilitätscheckUI <- function(id) {
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

#Server-------------------------------------------------
kompatibilitätscheckServer <- function(id, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      physikalische.Stabilität <- read.delim("./data/Salbenfibel/physikalische Stabilität.txt", header=TRUE)
      chemische.Stabilität <- read.delim("./data/Salbenfibel/chemische Stabilität.txt", header=TRUE)
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
      req(input$Salbengrundlage)
      if (sum(length(input$Salbengrundlage),length(input$Wirksubstanz)) != length(Bestandteile)){
        #the command setdiff(list.a, list.b) finds the non-overlapping elements only 
        #if these elements are contained in the object that is used as the first argument
        element_not_found <- setdiff(Bestandteile, c(input$Salbengrundlage, input$Wirksubstanz))
      }
    })
    
    
    element_not_kompatibel <- reactive({
      element_not_kompatibel_ph <- sub_physika_Stabil()[which(sub_physika_Stabil()$Mischbarkeit == "inkompatibel"),]$Substanz
      element_not_kompatibel_ch <- sub_chemi_Stabil()[which(sub_chemi_Stabil()$Mischbarkeit == "inkompatibel"),]$Substanz
      element_not_kompatibel <- unique(c(element_not_kompatibel_ph,  element_not_kompatibel_ch))
      element_not_kompatibel
    })
    
    element_kompatibel <- reactive({
      element_kompatibel_ph <- sub_physika_Stabil()[which(sub_physika_Stabil()$Mischbarkeit == "kompatibel"),]$Substanz
      element_kompatibel_ch <- sub_chemi_Stabil()[which(sub_chemi_Stabil()$Mischbarkeit == "chemisch stabil"),]$Substanz
      element_kompatibel <- c(element_kompatibel_ph,  element_kompatibel_ch)
      element_kompatibel
    })
    
    kompatibel <- reactive({
      length(element_kompatibel()) == sum(nrow(sub_physika_Stabil()),nrow(sub_chemi_Stabil()))
    })
    
    
    
    
    list(
      element_not_found = reactive(element_not_found()),
      element_not_kompatibel = reactive(element_not_kompatibel()),
      element_kompatibel = reactive(element_kompatibel()),
      kompatibel = reactive(kompatibel())
    )
    
    })
}

# #Test module:
# 
# KombatibilitätscheckApp <- function() {
# ui <- fluidPage(
#   kompatibilitätscheckUI("Salbenfibel"),
#   textOutput("kc"),
#   textOutput("komp")
# )
# 
# server <- function(input, output, session) {
#   #Bestandteile <- c("Zinkoxid", "Ultrasicc (R)", "blabla")
#   #Bestandteile <- c("Ultralip (R)", "Dexpanthenol")
#   Bestandteile <- c()
#   kc <- kompatibilitätscheckServer("Salbenfibel", Bestandteile)
#   output$kc <- renderText({
#     kc$element_kompatibel()
#   })
#   output$komp <- renderText({
#     kc$kompatibel()
#   })
# 
# 
# }
# 
# shinyApp(ui, server)}
# KombatibilitätscheckApp()

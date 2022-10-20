#02/07/2022
#1. read in Juniormed Rezeptursammlung convert it to dataframe
#2. (optional) upload, unzip, read in of private Rezeptursammlung
#3. merge Juniormed with optional uploaded private Rezeptursammlung.
#4. outputs Rezeptursammlungdataset and datapath of uploaded file

# inputs: ./data/Rezeptursammlung/Juniormed/Rezeptursammlung.txt, optional(interner zip ordner containing a Rezeptursammlung)
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

createRezeptursammlungUI <- function(id) {
  tagList(
    
    actionBttn(NS(id,"neue_rzs"), "interne Rezeptursammlung erstellen",style = "fill", color = "success", size = "s", block = T),
    tags$br(),
    fileInput(NS(id, "file"), 
              label = h4("interne Sammlung",
                         tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                         bsButton(NS(id,"q1"), label = "", icon = icon("question"), style = "info", size = "extra-small")
                         
                         ), accept = ".zip"),
    bsPopover(NS(id,"q1"), title = "interne Sammlung",
              content = paste0("lade deine eigene Sammlung an Rezepturen, Herstellungshinweise und Verdrängungsfaktoren als zip Ordner hoch. Haben Sie noch keine Sammlung? Dann klicken Sie auf: interne Rezeptursammlung erstellen"
              ),
              placement = "right", 
              trigger = "focus"),
    
  )
}


#Server-------------------------------------------------

createRezeptursammlungServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    datapath <- reactiveVal()
    
 
    
    observeEvent(input$neue_rzs, {
      datapath("./data/interne_Rezeptursammlung_leer.zip")
      #shinyjs::toggleState("file")
      #browser()
      hide("file")
    })
    
    observeEvent(input$file,{
      datapath(input$file$datapath)
      hide("neue_rzs")
    })
    
    #readin interne Rezeptursammlung
    interne_Rezeptursammlung <- reactive({
      req(datapath())
       dataSet <- zip2dataSet(datapath(), filenr = 1)
       dataSet <- adorigin2dataframe(dataSet,2)
       dataSet
      })
    
    
    interne_Herstellungshinweise <- reactive({
      req(datapath())
      dataSet <- zip2dataSet(datapath(), filenr = 2, header=F, sep = ";")
      dataSet
    })

    #merge juniormed with interne if uploaded
    rezeptursammlung <- reactive({
      if (!is.null(datapath())) {
        rezeptursammlung_Jun <- read.csv("./data/Rezeptursammlung/Juniormed/Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
        dataSet <- rbind(interne_Rezeptursammlung(), rezeptursammlung_Jun)
      } else {
        rezeptursammlung_Jun <- read.csv("./data/Rezeptursammlung/Juniormed/Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
      }
    })
    
    #return rezeptursammlung_dataset and datapath
    list(
      rezeptursammlung = reactive(rezeptursammlung()),
      datapath = reactive(datapath()),
      interne_Rezeptursammlung = reactive(interne_Rezeptursammlung()),
      interne_Herstellungshinweise = reactive(interne_Herstellungshinweise())
    )

  })
}





#Test module:

# RezeptursammlungApp <- function() {
#   ui <- fluidPage(
#     createRezeptursammlungUI("jun_and_int"),
#     uiOutput("selectizeInput01"),
#     tableOutput("table"),
#     textOutput("text2")
#     
#   )
# 
#   server <- function(input, output, session) {
#     #create ui to select Substanzen from sammlung
#     rz <- createRezeptursammlungServer("jun_and_int")
#     # rezeptursammlung <- rz$rezeptursammlung()   # browser()
#     output$selectizeInput01 <- renderUI({
# 
#          selectizeInput("Substanz", "Zusammensetzung der Rezeptur",choices = rz$rezeptursammlung()$V2, multiple = TRUE,
#                         options = list(placeholder = "wähle Substanzen aus"))
# 
# 
#     })
#   
#   output$text <- renderText(input$Substanz)
#   output$text2 <- renderText(rz$datapath())
#   # output$table <- renderTable(rezeptursammlung())
#   }
# 
#   shinyApp(ui, server)
# }
# 
#  RezeptursammlungApp()

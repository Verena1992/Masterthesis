#02/07/2022
#1. read in Juniormed Rezeptursammlung convert it to dataframe
#2. (optional) upload, unzip, read in of private Rezeptursammlung
#3. merge Juniormed with optional uploaded private Rezeptursammlung.
#4. outputs Rezeptursammlungdataset and datapath of uploaded file

# inputs: ./Rezeptursammlung.txt, optional(interner zip ordner containing a Rezeptursammlung)
# outputs: rezeptursammlungdataset and optional(datapath from uploaded interner Ordner)
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





#UI-----------------------------------------------------

createRezeptursammlungUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), 
              label = h4("interne Sammlung",
                         tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                         bsButton(NS(id,"q1"), label = "", icon = icon("question"), style = "info", size = "extra-small")
                         
                         ), accept = ".zip"),
    bsPopover(NS(id,"q1"), title = "interne Sammlung",
              content = paste0("lade deine eigene Sammlung an Rezepturen, Herstellungshinweise und Verdrängungsfaktoren als zip Ordner hoch. Haben Sie noch keine Sammlung? Dann klicken Sie auf erstellen"
              ),
              placement = "right", 
              trigger = "focus")
  )
}


#Server-------------------------------------------------

createRezeptursammlungServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    #readin interne Rezeptursammlung
    interne_Rezeptursammlung <- reactive({
       req(input$file)
       dataSet <- zip2dataSet(input$file$datapath, filenr = 1)
       dataSet <- adorigin2dataframe(dataSet,2)
       dataSet
      })

    #merge juniormed with interne if uploaded
    rezeptursammlung <- reactive({
      if (!is.null(input$file)) {
        rezeptursammlung_Jun <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
        dataSet <- rbind(interne_Rezeptursammlung(), rezeptursammlung_Jun)
      } else {
        rezeptursammlung_Jun <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
      }
    })
    
    #return rezeptursammlung_dataset and datapath
    list(
      rezeptursammlung = reactive(rezeptursammlung()),
      datapath = reactive(input$file$datapath)
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

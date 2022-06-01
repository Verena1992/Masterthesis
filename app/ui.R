library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)


rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")

Wirkstoff <- c()
Verdrängungsfaktor <- c()


# Home-------------------------------------------------------------
ui_Home <- tabPanel("Home", 
                    
                    fileInput("Verdrängungsfaktoren", "Choose CSV File"),
                              
                    logoutButton())


# Rezeptpflicht-----------------------------------------------------
ui_Rezeptpflichtcheck <- tabPanel("Rezeptpflichtcheck", 
                                  selectizeInput("WS", "Wirkstoff",choices = NULL),
                                  textOutput("Rstatus"))


# Suppositorien----------------------------------------------------------
ui_Suppositorien_gespeichert <- tabPanel("mit abgespeicherten Verdrängungsfaktoren",
                                         
                                         #selectizeInput("WS_S", "Substanz",choices = NULL),
                                         
                                         #only appear if substanz not in List
                                         #conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                          #                selectizeInput("New_Substanz", "Füge neue Substanz hinzu", choices = NULL)),
                                         #conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                          #                numericInput("New_Verdrängungsfaktor", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                          #                actionButton("Substanz_hinzufügen", "Substanz und Verdrängungsfaktor in Liste abspeichern"),
                                          #                downloadButton("download")),
                                         #tableOutput("New_Substanz")
                                        
                                         )


ui_Suppositorien <- tabPanel("ohne abgespeicherten Verdrängungsfaktoren", 
                             fluidPage(
                              # titlePanel("Hartfettmengenrechner"),
                              # tags$hr(),
                             fluidRow(
                             column(4,
                             numericInput("Stückanzahl", "Stückanzahl", value = 6, min = 0, max = 50, step = 1), 
                             numericInput("Eichwert", "Eichwert", value = 2, min = 0, max = 3, step = 0.01), 
                             numericInput("Überschuss", "Überschuss(%)", value = 10, min = 0, max = 100, step = 2)
                             ),
                             column(4,
                                    conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                                     selectizeInput("New_Substanz", "Füge neue Substanz hinzu", choices = NULL)),
                                    conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                                     numericInput("New_Verdrängungsfaktor", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                                     actionButton("Substanz_hinzufügen", "übernehmen")
                                                     )
                                    
                                    ),
                             column(4,
                                    tableOutput("New_Substanz")
                                    )
                             ),
                             
                             fluidRow(
                             column(4,
                             wellPanel(tags$h4("Substanz 1"), 
                                       conditionalPanel("output.fileUploaded",
                                                        selectizeInput("WS_S", "Substanz",choices = NULL),
                                                        htmlOutput("vf"),
                                                        tags$hr()),
                                       
                                       
                                       conditionalPanel("!output.fileUploaded", 
                                                        numericInput("Vf", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01)),
                                       numericInput("Menge_Substanz1", "Menge(g) pro Zäpfchen", value = 0.7, min = 0, max = 1, step = 0.01), 
                                       actionButton("weitere_Substanz", "weitere Substanz hinzufügen") 
                                       
                             )), 
                             column(4,
                                       conditionalPanel(condition = "input.weitere_Substanz > input.Substanz2_entfernen", 
                                       wellPanel(tags$h4("Substanz 2"),
                                       numericInput("Vf2", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01),
                                       numericInput("Menge_Substanz2", "Menge(g)", value = 0.7, min = 0, max = 1, step = 0.01),
                                       actionButton("weitere_Substanz2", "weitere Substanz hinzufügen"),
                                       actionButton("Substanz2_entfernen", "Substanz entfernen"),
                                                                 ))),
                             column(4,
                                       conditionalPanel(condition = "input.weitere_Substanz2 > input.Substanz3_entfernen", 
                                       wellPanel(tags$h4("Substanz 3"),
                                       numericInput("Vf3", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01),
                                       numericInput("Menge_Substanz3", "Menge(g)", value = 0.7, min = 0, max = 1, step = 0.01),
                                       actionButton("Substanz3_entfernen", "Substanz entfernen")))
                                              
                                              
                             )),
                             
                             wellPanel( 
                               fluidRow(
                                 column(6,
                             actionButton("Berechnung_Menge", "Berechnen", class = "btn-primary btn-lg" )),
                               fluidRow(
                                 column(6,
                             conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                             downloadButton("download", label = "Aktualiesierte Verdrängungsfaktoren herunterladen")))))
                             )
                             )
                             #textOutput("nötige_Hartfettmenge")
                             #class = "btn-primary btn-lg"
)

ui <- navbarPage("My Application", ui_Home, ui_Rezeptpflichtcheck, navbarMenu("Suppositorien-Hartfettmengenrechner",ui_Suppositorien_gespeichert, ui_Suppositorien)
                 )


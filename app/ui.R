library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
library(shinyFiles)

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
juniormed_pagenr <- readRDS("~/data/Juniormed/juniormed_pagenr.rds")

Wirkstoff <- c()
Verdrängungsfaktor <- c()

#Rezeptur hinzufügen----------------------------------------------------------------------------------------------------

#Titel;Anwendung;Haltbarkeit;Lagerung; Dosierung;Herstellungshinweise;Quelle
ui_Rezepturhinzufügen <- tabPanel(title = "neue Rezeptur", value = "Rezepturhinzufügen",
                                  fluidPage(
                                    titlePanel("Eigene Rezeptur hinzufügen"), 
                                    tags$br(),
                                    textAreaInput("Titel", "Titel", width = "100%"), 
                                    textAreaInput("Herstellungshinweise", "Herstellungshinweise", height = "200px", width = "100%"),
                                    textAreaInput("Quelle", "Quelle", width = "100%"), 
                                    textAreaInput("Dosierung", "Dosierung", width = "100%"),
                                    textAreaInput("Haltbarkeit", "Haltbarkeit", width = "100%"),
                                    textAreaInput("Lagerung", "Lagerung", width = "100%"),
                                    textAreaInput("Anwendung", "Anwendung", width = "100%"),
                                    actionBttn(
                                      inputId = "eigeneRezeptur_hinzu",
                                      label = "Rezeptur hinzufügen",
                                      color = "warning",
                                      style = "fill", 
                                      size = "lg", 
                                      block = TRUE
                                    ),
                                    tags$br()
                                  ))







# Home-------------------------------------------------------------
ui_Home <- tabPanel("Home", 
                    
                    fileInput("Verdrängungsfaktoren", "Choose CSV File"),
                   # fileInput("interne_Rezeptursammlung", "Rezeptursammlung"),
                    fileInput("file", "Upload Zip file", accept = ".zip"),
                   tableOutput("new_Rezeptur"),
                   downloadButton("download_newRezeptur", label = "Neue Verdrängungsfaktoren zur Liste hinzufügen"),       
                    logoutButton())

# Rezeptursammlung----------------------------------------------------------
ui_Rezeptursammlung <- tabPanel("Rezeptursammlung", 
                                fluidPage(
                                  titlePanel("Rezeptursammlung"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      tags$h3("NRF"),
                                      actionButton("NRF_online", "NRF_online suchen"),
                                      tags$hr(),
                                      
                                      tags$h3("Juniormed"),
                                      
                                      actionBttn(
                                        inputId = "plus",
                                        label = "Substanz hinzufügen",
                                        color = "success",
                                        style = "bordered", 
                                        size = "xs"
                                      ),
                                      
                                      actionBttn(
                                        inputId = "minus",
                                        label = "Substanz entfernen",
                                        color = "royal",
                                        style = "bordered", 
                                        size = "xs"
                                      ),
                                      
                                      tags$hr(),
                                      #selectizeInput("WS_Sammlung", "1.Substanz",choices = NULL),
                                      uiOutput("moreSubstanzen"),
                                      
                                      actionButton("Juniormed", "Juniormed suchen"),
                                      
                                      tags$hr(),
                                      conditionalPanel(condition="output.interne_Rezeptursammlung", 
                                      tags$h3("eigene Rezeptursammlung"),
                                      actionBttn(
                                        inputId = "eR_plus",
                                        label = "Substanz hinzufügen",
                                        color = "success",
                                        style = "bordered", 
                                        size = "xs"
                                      ),actionBttn(
                                        inputId = "eR_minus",
                                        label = "Substanz entfernen",
                                        color = "royal",
                                        style = "bordered", 
                                        size = "xs"
                                      ),
                                      tags$hr(),
                                      #selectizeInput("WS_Sammlung", "1.Substanz",choices = NULL),
                                      uiOutput("moreSubstanzen_2"),
                                      
                                      actionButton("ei_Rezeptur_B", "suchen"),
                                      tags$hr(),
                                      actionBttn(inputId = "jumpto_neueRezep", label = "neue Rezeptur zur Sammlung hinzufügen", size = "xs", style = "bordered", color = "primary")
                                      ))
                                      ,
                                    mainPanel(conditionalPanel(condition = "input.NRF_online", 
                                      tags$iframe(src="https://dacnrf.pharmazeutische-zeitung.de/dac/nrf-wissen/rezepturenfinder/offen", height=500, width=800)),
                                      #tableOutput("table"),
                                      
                                      uiOutput("Rezepturen"),
                                      uiOutput("Rezepturen_int"),
                                      uiOutput("Herstellungshinweis"),
                                      tableOutput("Herstellungstext_int")
                                      
                                             )
                                    #textOutput("table")
                                       
                                  )))
                               # tags$iframe(src="https://dacnrf.pharmazeutische-zeitung.de/dac/nrf-wissen/rezepturenfinder/offen", height=300, width=800), 
                              #  tags$h2("Juniormed"), 
                              #  selectizeInput("WS_Sammlung", "1.Substanz",choices = NULL))    
                      

# Rezeptpflicht-----------------------------------------------------
ui_Rezeptpflichtcheck <- tabPanel(title = "Rezeptpflichtcheck", value = "Rezeptpflichtcheck",
                                  selectizeInput("WS", "Wirkstoff",choices = NULL),
                                  textOutput("Rstatus"), 
                                 # tags$iframe(src="http://juniormed.at/pdf/#kompendium/5", height=500, width=800),
                                 # uiOutput("Herstellungshinweis")
                                 )


# Suppositorien----------------------------------------------------------
ui_Suppositorien_gespeichert <- tabPanel("Dosierungcheck",
                                         
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


ui_Suppositorien_Rechner <- tabPanel("Hartfettmengenrechner", 
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
                                    conditionalPanel(condition = "input.WS_S == 'Substanz nicht in Liste vorhanden' 
                                                     && input.weitere_Substanz <= input.Substanz2_entfernen"
                                                     ,
                                                     selectizeInput("New_Substanz", "Arzneimittel, Arzneitaxe-Anlage B", choices = NULL),
                                    
                                                     numericInput("New_Verdrängungsfaktor", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                                     actionButton("Substanz_hinzufügen", "übernehmen")),
                                    conditionalPanel(condition = "input.WS_S2 == 'Substanz nicht in Liste vorhanden' 
                                                     && input.weitere_Substanz2 <= input.Substanz3_entfernen
                                                     && input.Substanz2_entfernen < input.weitere_Substanz",
                                                     selectizeInput("New_Substanz2", "Arzneimittel, Arzneitaxe-Anlage B", choices = NULL),
                                                     
                                                     numericInput("New_Verdrängungsfaktor2", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                                     actionButton("Substanz_hinzufügen2", "übernehmen")),
                                    
                                    conditionalPanel(condition = "input.WS_S3 == 'Substanz nicht in Liste vorhanden' 
                                                     && input.weitere_Substanz2 >= input.Substanz3_entfernen
                                                     && input.Substanz3_entfernen < input.weitere_Substanz2",
                                                     selectizeInput("New_Substanz3", "Arzneimittel, Arzneitaxe-Anlage B", choices = NULL),
                                                     
                                                     numericInput("New_Verdrängungsfaktor3", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                                     actionButton("Substanz_hinzufügen3", "übernehmen")),
                                    
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
                                                       
                                                        conditionalPanel(condition = "input.WS_S == 'Substanz nicht in Liste vorhanden'",
                                                                         tags$h4(htmlOutput("sub")) ),
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
                                       conditionalPanel("output.fileUploaded",
                                                        selectizeInput("WS_S2", "Substanz",choices = NULL),
                                                        
                                                        conditionalPanel(condition = "input.WS_S2 == 'Substanz nicht in Liste vorhanden'",
                                                                         tags$h4(htmlOutput("sub2")) ),
                                                        htmlOutput("vf2"),
                                                        tags$hr()),
                                       conditionalPanel("!output.fileUploaded",
                                                        numericInput("Vf2", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01)),
                                       numericInput("Menge_Substanz2", "Menge(g) pro Zäpfchen", value = 0.7, min = 0, max = 1, step = 0.01),
                                       actionButton("weitere_Substanz2", "weitere Substanz hinzufügen"),
                                       actionButton("Substanz2_entfernen", "Substanz entfernen"),
                                                                 ))),
                             column(4,
                                       conditionalPanel(condition = "input.weitere_Substanz2 > input.Substanz3_entfernen", 
                                       wellPanel(tags$h4("Substanz 3"),
                                       conditionalPanel("output.fileUploaded",
                                                        selectizeInput("WS_S3", "Substanz",choices = NULL),
                                                       
                                                        conditionalPanel(condition = "input.WS_S3 == 'Substanz nicht in Liste vorhanden'",
                                                                         tags$h4(htmlOutput("sub3") )),
                                                        htmlOutput("vf3"),
                                                                  tags$hr()),
                                       conditionalPanel("!output.fileUploaded",      
                                                        numericInput("Vf3", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01)),
                                       numericInput("Menge_Substanz3", "Menge(g) pro Zäpfchen", value = 0.7, min = 0, max = 1, step = 0.01),
                                       actionButton("Substanz3_entfernen", "Substanz entfernen")))
                                              
                                              
                             )),
                             
                             wellPanel( 
                               fluidRow(
                                 column(6,
                             actionButton("Berechnung_Menge", "Berechnen", class = "btn-primary btn-lg" )),
                               fluidRow(
                                 column(6,
                                        #input.WS_S == 'Substanz nicht in der Liste vorhanden'
                             conditionalPanel(condition = "input.Substanz_hinzufügen > 0",
                             downloadButton("download", label = "Neue Verdrängungsfaktoren zur Liste hinzufügen")))))
                             )
                             )
                             #textOutput("nötige_Hartfettmenge")
                             #class = "btn-primary btn-lg"
)


ui <- navbarPage("My Application", id = "inTabset", ui_Home,
                 navbarMenu("Rezeptursammlung",ui_Rezeptursammlung, ui_Rezepturhinzufügen ),
                 
                 navbarMenu("Suppositorien", ui_Suppositorien_Rechner, ui_Suppositorien_gespeichert),
                 ui_Rezeptpflichtcheck)



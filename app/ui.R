#--------------------------------------------------------------------------
#load_libraries()

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
#----------------------------------------------------------------------------
rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")



Wirkstoff <- c()
Verdrängungsfaktor <- c()

# Home-------------------------------------------------------------
ui_Home <- tabPanel("Home", 
                    fluidPage(
                      sidebarLayout(
                        sidebarPanel(
                    tags$h2("Uploads"),
                    createRezeptursammlungUI("jun_and_int"),
                    createVerdrängungsfaktorenUI("nrf_and_int"),
                    #tags$hr(),
                    tags$h2("Downloads"),
                    tags$h5("Neue Informationen zur interner Sammlung hinzufügen und herunterladen"),
                    downloadButton("download_newRezeptur", label = "Download"),
                    #actionButton("NRF_online", "NRF online suchen"),
                    tags$hr(),
                    logoutButton()
                        ),
                    mainPanel(
                    #tags$hr(),
                      conditionalPanel(condition = "input.eigeneRezeptur_hinzu < 1",
                        tags$h1("Willkommen"),
                      tags$hr(),
                      img(src='pharmazie.png',height = '172px', width = '200px')),#, align = "right"),  
                      conditionalPanel(condition = "input.eigeneRezeptur_hinzu",
                      tags$h3("Neue Informationen")),
                      
                    tableOutput("new_Herstellungshinweis"),
                    tableOutput("new_Verdrängungsfaktor"),
                      
                    
                    ))))



# Rezeptursammlung----------------------------------------------------------
ui_Rezeptursammlung <- tabPanel("Rezeptursammlung", 
                                fluidPage(
                                  titlePanel("Rezeptursammlung"),
                                  sidebarLayout(
                                    
                                    sidebarPanel(
                                     # selectizeInput("zusammensetzungRezep", "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                                    #                 options = list(placeholder = "wähle Substanzen aus")),
                                      uiOutput("selectizeInput01"),
                                      tags$h3("NRF"),
                                      actionButton("NRF_online", "NRF online suchen"),
                                      tags$hr(),
                                      uiOutput("erstattungscheck"), 
                                      tags$hr(),
                                      uiOutput("kompatibilität"), 
                                      uiOutput("hartfettberechnen"), 
                                      uiOutput("bedenklicher_Stoff"),
                                      textOutput("bs"), 
                                      uiOutput("dosierung")
                                    ),
                                    
                                    mainPanel(
                                      conditionalPanel(condition = "input.NRF_online", 
                                                      tags$iframe(src="https://dacnrf.pharmazeutische-zeitung.de/dac/nrf-wissen/rezepturenfinder/offen", height=500, width=800)
                                                      ),
                                              
                                      foundRezepturenButtonUI("button"),
                                    )
                                  )
                                )
                              )

#--------------------------------------------------------------------------


#neue_Zusammensetzung_Rezeptur----------------------------------------------------------------------------------------------------
ui_neue_Zusammensetzung_Rezeptur <- tabPanel(title = "neue Rezeptur", value = "neue_Zusammensetzung_Rezeptur",
                                      fluidPage(
                                        titlePanel("Neue Rezeptur - Zusammensetzung"),
                                        addRezepturUI("Zusammensetzung"),
                                        tableOutput("text2")
                                       )
                                      )

#--------------------------------------------------------------------------



#Rezeptur hinzufügen----------------------------------------------------------------------------------------------------

ui_Rezepturhinzufügen <- tabPanel(title = "neue Herstellungsanweisung", value = "Rezepturhinzufügen",
                                  fluidPage(
                                    titlePanel("Eigene Rezeptur hinzufügen"), 
                                    tags$br(),
                                    rezepturhinweiseUI("textAreas"),
                                    big_yellow_button("eigeneRezeptur_hinzu", "Rezeptur hinzufügen"),
                                    tags$br()
                                  ))


# Erstattungscheck---------------------------------------------------

ui_Erstattungscheck <- tabPanel(title = "Erstattungscheck", value = "Erstattungscheck",
                                fluidPage(
                                
                                titlePanel("Erstattungscheck"), 
                                tags$hr(),
                                
                                fluidRow(
                                  column(5,
                                actionBttn("reset_ec", "Reset ausgewählte Rezeptur"),
                                #tags$hr(),
                                erstattungscheckUI("ec")), 
                                  column(6,
                                         conditionalPanel(condition = "output.eng",
                                                          wellPanel(style = "background: red",
                                                                    tags$h3("Achtung!"),
                                                                    tags$h5("folgende Substanz(en) befinden sich nicht in der grünen Box:"),
                                                                    tags$h2(textOutput("eng"))
                                                          )    
                                                          
                                         ),
                                  conditionalPanel(condition = "output.enf", 
                                    wellPanel(style = "background: yellow",
                                      tags$h3("Achtung!"),
                                      tags$h5("von der ausgewählter Rezeptur wurden folgende Substanz(en) nicht gefunden:"), 
                                      tags$hr(),
                                      tags$h2(textOutput("enf")), 
                                      tags$hr(),
                                      tags$h6("Kontrolliere ob ein Synonym in der Arzneitaxe gelistet ist!")
                                    ))
                              
                                  
                                  )
                                ))
)



                                 
                      

# Rezeptpflicht-----------------------------------------------------
ui_Rezeptpflichtcheck <- tabPanel(title = "Rezeptpflichtcheck", value = "Rezeptpflichtcheck",
                                  selectizeInput("WS", "Wirkstoff",choices = NULL),
                                  textOutput("Rstatus"), 
                               
                                 )


# Suppositorien----------------------------------------------------------
ui_Suppositorien_gespeichert <- tabPanel("Dosierungcheck",)


ui_Suppositorien_Rechner <- tabPanel("Hartfettmengenrechner", value = "Hartfettmengenrechner",
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
                                    #tableOutput("New_Substanz")
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
                              actionButton("Berechnung_Menge", "Berechnen", class = "btn-primary btn-lg" )
                             )
                             )
                       
)
#Kombatibilitätscheck---------------------------------------------------------
ui_Kombatibilitätscheck <- tabPanel("Kompatibilitätscheck", value = "Kombatibilitätscheck",
                                    fluidPage(
                                      titlePanel("Kompatibilitätscheck"), 
                                      tags$hr(),
                                      fluidRow(
                                        column(7,
                                      kompatibilitätscheckUI("Salbenfibel"),
                                      textOutput("kc")),
                                        column(5,
                                      conditionalPanel(condition = "output.enf_kc", 
                                                       wellPanel(style = "background: yellow",
                                                                 tags$h3("Achtung!"),
                                                                 tags$h5("von der ausgewählter Rezeptur wurden folgende Substanz(en) nicht gefunden:"), 
                                                                 tags$hr(),
                                                                 tags$h2(textOutput("enf_kc")), 
                                                                 tags$hr(),
                                                                 tags$h6("Kontrolliere ob ein Synonym gelistet ist!")
                                                       ))))
                                    )
                                    ) 
#bedenkliche Stoffe---------------------------------------------------------

ui_bedenkliche_Stoffe <- tabPanel("bedenkliche Stoffe", value = "bedenkliche Stoffe",
                                  fluidPage(
                                    titlePanel("Bedenkliche Rezepturarzneimittel"), 
                                    tags$hr(),
                                    bedenklichStUI("arzneimittelkommission"),
                                  )
                                  )
#Dosierungscheck------------------------------------------------------------------


ui_Dosierungscheck <- tabPanel("Dosierungscheck", value = "Dosierungscheck",
                                  fluidPage(
                                    titlePanel("Dosierung der Wirkstoffe zur Lokalanwendung"), 
                                    tags$hr(),
                                    dosierungUI("dosierung"), 
                                    conditionalPanel(condition = "output.limits",
                                    wellPanel(style = "background: red",
                                     #         tags$h3("Achtung!"),
                                    textOutput("limits")))
                                    
                                  )
)


#ui <- auth0_ui(navbarPage("My Application
ui <- navbarPage("My Application", id = "inTabset", ui_Home,
                 navbarMenu("Rezeptursammlung",ui_Rezeptursammlung, ui_neue_Zusammensetzung_Rezeptur, ui_Rezepturhinzufügen ),
                 ui_Erstattungscheck,
                 navbarMenu("Suppositorien", ui_Suppositorien_Rechner, ui_Suppositorien_gespeichert),
                 ui_Rezeptpflichtcheck, ui_Kombatibilitätscheck, ui_bedenkliche_Stoffe, ui_Dosierungscheck)



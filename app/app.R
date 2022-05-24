library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")

Wirkstoff <- c()
Verdrängungsfaktor <- c()

ui_Rezeptpflichtcheck <- tabPanel("Rezeptpflichtcheck", 
                                  selectizeInput("WS", "Wirkstoff",choices = NULL),
                                  textOutput("Rstatus"))


ui_Suppositorien_gespeichert <- tabPanel("mit abgespeicherten Verdrängungsfaktoren",
                            fileInput("Verdrängungsfaktoren", "Choose CSV File", accept = c(
                            "text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")
                            ),
                            selectizeInput("WS_S", "Substanz",choices = NULL),
                            
                            #only appear if substanz not in List
                            conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                             selectizeInput("New_Substanz", "Füge neue Substanz hinzu", choices = NULL)),
                            conditionalPanel(condition = "input.WS_S == 'Substanz nicht in der Liste vorhanden'",
                                             numericInput("New_Verdrängungsfaktor", "Füge neuen Verdrängungsfaktor hinzu", value = 0.7, min = 0, max = 1), 
                                             actionButton("Substanz_hinzufügen", "Substanz und Verdrängungsfaktor in Liste abspeichern"),
                                             downloadButton("download")),
                            tableOutput("New_Substanz"))


ui_Suppositorien <- tabPanel("ohne abgespeicherten Verdrängungsfaktoren", 
                             numericInput("Stückanzahl", "Stückanzahl", value = 6, min = 0, max = 50, step = 1), 
                             numericInput("Eichwert", "Eichwert", value = 2, min = 0, max = 3, step = 0.01), 
                             numericInput("Überschuss", "Überschuss(%)", value = 10, min = 0, max = 100, step = 2),
                             wellPanel(tags$h4("Substanz 1"), 
                             numericInput("Vf", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01),
                             numericInput("Menge_Substanz1", "Menge(g)", value = 0.7, min = 0, max = 1, step = 0.01), 
                             actionButton("weitere_Substanz", "weitere Substanz hinzufügen") 
                             
                             ), 
                             
                             conditionalPanel(condition = "input.weitere_Substanz > input.Substanz2_entfernen", 
                                              wellPanel(tags$h4("Substanz 2"),
                                              numericInput("Vf2", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01),
                                              numericInput("Menge_Substanz2", "Menge(g)", value = 0.7, min = 0, max = 1, step = 0.01),
                                              actionButton("weitere_Substanz2", "weitere Substanz hinzufügen"),
                                              actionButton("Substanz2_entfernen", "Substanz entfernen"),
                                              ),
                                              conditionalPanel(condition = "input.weitere_Substanz2 > input.Substanz3_entfernen", 
                                                               wellPanel(tags$h4("Substanz 3"),
                                                               numericInput("Vf3", "Verdrängungsfaktor", value = 0.7, min = 0, max = 1, step = 0.01),
                                                               numericInput("Menge_Substanz3", "Menge(g)", value = 0.7, min = 0, max = 1, step = 0.01),
                                                               actionButton("Substanz3_entfernen", "Substanz entfernen")))
                                             
                                              
                             )
                             ,actionButton("weitere_Substanz", "weitere Substanz hinzufügen"), 
                             textOutput("nötige_Hartfettmenge")
                             )

ui <- navbarPage("My Application", ui_Rezeptpflichtcheck, navbarMenu("Suppositorien-Hartfettmengenrechner",ui_Suppositorien_gespeichert, ui_Suppositorien),
                 tabPanel("Component 3")
)



#ui <- fluidPage(
#  selectizeInput("WS", "Wirkstoff",choices = NULL),
#  textOutput("Rstatus")
  
#  )


server <- function(input, output, session) {
# Upload Verdrängungsfaktoren-----------------------------------------------------
  
  data_Verdrän <- reactive({
    req(input$Verdrängungsfaktoren)#to make sure code waits until the first file is uploaded
    #first column = c(character), second = double
    #verdrängungsfaktor needs to be written with point as comma
    #datapath = The path to a temp file that contains the data that was uploaded
    dataSet <- vroom::vroom(input$Verdrängungsfaktoren$datapath, delim = "\t", col_types = "cd")
    
    })
  
  observe({
    dataSet <- data_Verdrän()
    updateSelectizeInput(session, "WS_S", choices = c(dataSet$Wirkstoff, "Substanz nicht in der Liste vorhanden"))
    })
  
  updateSelectizeInput(session, "New_Substanz", choices = taxe_eko$wirkstoffe_arzneitaxe, server = TRUE)
  

  
  new_data <- eventReactive(input$Substanz_hinzufügen, {
    #assigning as global variable ("<<-")is needed to append
    Wirkstoff <<- append(Wirkstoff, input$New_Substanz)
    Verdrängungsfaktor <<- append(Verdrängungsfaktor, input$New_Verdrängungsfaktor)
    #new_dataSet <- vroom::vroom_write(list(input$New_Substanz, input$New_Verdrängungsfaktor), 
     #                                 input$Verdrängungsfaktoren$name, delim = "\t", append = T)
    
    #print(typeof(input$New_Verdrängungsfaktor))
    #new_dataSet[nrow(data_Verdrän()) + 1,] <- list(input$New_Substanz, input$New_Verdrängungsfaktor)
    new_data <- data.frame(Wirkstoff, Verdrängungsfaktor)
    new_data
    print(new_data)
  })

  
  
  
#  new_dataSet <- reactive({
    #print(input$New_Substanz)
#    new_dataSet <- data_Verdrän()
    #print(typeof(input$New_Verdrängungsfaktor))
#    new_dataSet[nrow(new_dataSet) + 1,] <- list(input$New_Substanz, input$New_Verdrängungsfaktor)
#    new_dataSet
#  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$Verdrängungsfaktoren)
    },
    content = function(file) {
      data_Verdrän <- rbind(new_data(),data_Verdrän())
      vroom::vroom_write(data_Verdrän[order(data_Verdrän$Wirkstoff),], 
                  file, delim = "\t")
    }
  )
  
  
  output$New_Substanz <- renderTable({
   new_data()
  })
  

  output$preview1 <- renderTable(head(data_Verdrän()))

#Harfettgrundmasse berechnen--------------------------------------------------------------
  
  output$nötige_Hartfettmenge <- renderText(input$Stückanzahl * input$Eichwert * input$Überschuss)

  
  
  
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )

  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezeptpflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })

# ohne abgespeicherten Verdrängungsfaktoren
  
  observeEvent(input$Stückanzahl,{
    req(input$Stückanzahl)
    if (input$Stückanzahl > 24) {
    # Show a simple modal
      shinyalert(title = "Bewilligung des chef- und kontrollärztlichen Dienstes nötig! Höchstmenge überschritten", type = "warning")}
  })

}

auth0::shinyAppAuth0(ui, server)
#shinyApp(ui = ui, server = server)

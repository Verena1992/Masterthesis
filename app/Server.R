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
  
  ## ohne abgespeicherten Verdrängungsfaktore
 
  
  
  observeEvent(input$Stückanzahl,{
    req(input$Stückanzahl)
    if (input$Stückanzahl > 24) {
      # Show a simple modal
      shinyalert(title = "Bewilligung des chef- und kontrollärztlichen Dienstes nötig! Höchstmenge überschritten", type = "warning")}
  })
  
  observeEvent(input$Berechnung_Menge,{
    #req(input$Stückanzahl)
    
      # Show a simple modal
      shinyalert(
        html = TRUE,
        title = "Berechnete Einwaagen",
        text = tagList(
         
          "Hartfettmenge:",
          textOutput("nötige_Hartfettmenge", inline = TRUE)
        ))
        
  }) 
  
  
  
  
  
  
  output$nötige_Hartfettmenge <- renderText({
    einwaage_ohne_WS <- input$Stückanzahl * input$Eichwert
    verdrängung1 <- (input$Menge_Substanz1 * input$Stückanzahl * input$Vf)
    verdrängung2 <- if(input$weitere_Substanz > input$Substanz2_entfernen){
      (input$Menge_Substanz2 * input$Stückanzahl * input$Vf)} else {0}
    verdrängung3 <- if(input$weitere_Substanz2 > input$Substanz3_entfernen){
      (input$Menge_Substanz3 * input$Stückanzahl * input$Vf)} else {0}
    verdrängung <- verdrängung1 + verdrängung2 + verdrängung3
                   
                  
    einwaage_mit_Überschuss <- (einwaage_ohne_WS - verdrängung)/100 * (input$Überschuss+100)
    #((input$Stückanzahl * input$Eichwert)-(input$Menge_Substanz1 *input$Stückanzahl * input$Vf))/100 * (input$Überschuss+100)
    einwaage_mit_Überschuss
    }) 
  
  
  
  
  
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )
  
  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezeptpflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })
  
  

  
  
  

  
  
        
}


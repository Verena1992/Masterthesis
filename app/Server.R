library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
Rezeptursammlung <- read.csv("~/Rezeptursammlung.txt", header=FALSE, sep=";")

Wirkstoff <- c()
Verdrängungsfaktor <- c()

server <- function(input, output, session) {
# Home-----------------------------------------------------
  
  data_Verdrän <- reactive({
    req(input$Verdrängungsfaktoren)#to make sure code waits until the first file is uploaded
    #first column = c(character), second = double
    #verdrängungsfaktor needs to be written with point as comma
    #datapath = The path to a temp file that contains the data that was uploaded
    ext <- tools::file_ext(input$Verdrängungsfaktoren$datapath)
    #validate(need(ext == "txt" | ext == "pdf", "Please upload a csv file"))
    if (ext == "txt"){
    dataSet <- vroom::vroom(input$Verdrängungsfaktoren$datapath, delim = "\t", col_types = "cd")
    } else if (ext == "pdf") {
      source("verdrängungsfaktoren_pdf.R")
      print("pdf")
      dataSet
    }
  })
  
  observe({
    dataSet <- data_Verdrän()
    updateSelectizeInput(session, "WS_S", choices = c(dataSet$Wirkstoff, "Substanz nicht in Liste vorhanden"))
  })
  
  observe({
    dataSet <- data_Verdrän()
    updateSelectizeInput(session, "WS_S2", choices = c(dataSet$Wirkstoff, "Substanz nicht in Liste vorhanden"))
  })
  
  observe({
    dataSet <- data_Verdrän()
    updateSelectizeInput(session, "WS_S3", choices = c(dataSet$Wirkstoff, "Substanz nicht in Liste vorhanden"))
  })
  
  
  
  output$fileUploaded <- reactive({
    return(!is.null(data_Verdrän()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$vf <- renderUI({
    dataSet <- data_Verdrän()
    vf <- dataSet[which(dataSet$Wirkstoff == input$WS_S),]
    if (input$Substanz_hinzufügen){
      vf <- input$New_Verdrängungsfaktor
    } else {
    vf <- vf$Verdrängungsfaktor}
    HTML(paste("Verdrägungsfaktor = ",vf))
  })
  
  output$vf2 <- renderUI({
    dataSet <- data_Verdrän()
    vf <- dataSet[which(dataSet$Wirkstoff == input$WS_S2),]
    if (input$Substanz_hinzufügen2){
      vf2 <- input$New_Verdrängungsfaktor2
    } else {
      vf2 <- vf$Verdrängungsfaktor}
    HTML(paste("Verdrägungsfaktor = ",vf2))
  })
  
  output$vf3 <- renderUI({
    dataSet <- data_Verdrän()
    vf <- dataSet[which(dataSet$Wirkstoff == input$WS_S3),]
    if (input$Substanz_hinzufügen3){
      vf3 <- input$New_Verdrängungsfaktor3
    } else {
      vf3 <- vf$Verdrängungsfaktor}
    HTML(paste("Verdrägungsfaktor = ",vf3))
  })
  
  
  
   
  
  
  output$sub <- renderUI({
    req(input$New_Substanz)
    input$New_Substanz
  })
  
  output$sub2 <- renderUI({
    req(input$New_Substanz2)
    input$New_Substanz2
  })  
  
  output$sub3 <- renderUI({
    req(input$New_Substanz3)
    input$New_Substanz3
  })
  
  
  
  
  
  updateSelectizeInput(session, "New_Substanz", choices = taxe_eko$wirkstoffe_arzneitaxe, server = TRUE)
  updateSelectizeInput(session, "New_Substanz2", choices = taxe_eko$wirkstoffe_arzneitaxe, server = TRUE)
  updateSelectizeInput(session, "New_Substanz3", choices = taxe_eko$wirkstoffe_arzneitaxe, server = TRUE)
          
  
  new_data <- eventReactive(list(input$Substanz_hinzufügen , input$Substanz_hinzufügen2, input$Substanz_hinzufügen3),{
    req(input$Substanz_hinzufügen | input$Substanz_hinzufügen2 | input$Substanz_hinzufügen3)
    #assigning as global variable ("<<-")is needed to append
    if (input$weitere_Substanz2 > input$Substanz3_entfernen) {
      Wirkstoff <<- append(Wirkstoff, input$New_Substanz3)
      Verdrängungsfaktor <<- append(Verdrängungsfaktor, input$New_Verdrängungsfaktor3)}
    else if (input$weitere_Substanz > input$Substanz2_entfernen){
      Wirkstoff <<- append(Wirkstoff, input$New_Substanz2)
      Verdrängungsfaktor <<- append(Verdrängungsfaktor, input$New_Verdrängungsfaktor2)}
    else {
    Wirkstoff <<- append(Wirkstoff, input$New_Substanz)
    Verdrängungsfaktor <<- append(Verdrängungsfaktor, input$New_Verdrängungsfaktor)}

    new_data <- data.frame(Wirkstoff, Verdrängungsfaktor)
    new_data
    #print(new_data)
  })
  
 
  
  
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
    #new_data <- data.frame(Wirkstoff, Verdrängungsfaktor)
    #browser()
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
      shinyalert(
        html = TRUE,
        title = "Berechnete Einwaagen(g)",
        text = tagList(
         
          tags$h3("Hartfett:"),
          textOutput("nötige_Hartfettmenge", inline = T),
          tags$hr(),
          tags$h3("Substanz 1:"),
          textOutput("nötige_Substanzmenge1", inline = T),
          tags$hr(),
          if(input$weitere_Substanz > input$Substanz2_entfernen){
            tags$h3("Substanz 2:")},
          if(input$weitere_Substanz > input$Substanz2_entfernen){
            textOutput("nötige_Substanzmenge2", inline = T)},
          if(input$weitere_Substanz2 > input$Substanz3_entfernen){
            tags$hr()},          
          if(input$weitere_Substanz2 > input$Substanz3_entfernen){
            tags$h3("Substanz 3:")},
          if(input$weitere_Substanz2 > input$Substanz3_entfernen){
            textOutput("nötige_Substanzmenge3", inline = T)}         
          ))})
  
  
  
  
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
  
  output$nötige_Substanzmenge1 <- renderText({
    (input$Menge_Substanz1 * input$Stückanzahl)/100 * (input$Überschuss+100)
  })
  
  output$nötige_Substanzmenge2 <- renderText({
    (input$Menge_Substanz2 * input$Stückanzahl)/100 * (input$Überschuss+100)
  })
 
  output$nötige_Substanzmenge3 <- renderText({
    (input$Menge_Substanz3 * input$Stückanzahl)/100 * (input$Überschuss+100)
  })
  
 
  
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )
  
  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezeptpflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })

  
# Rezeptursammlung----------------------------------------------------------   

  
  value <- reactiveVal(1)       # rv <- reactiveValues(value = 0)
  
  observeEvent(input$minus, {
    if (value() > 1) {
    newValue <- value() - 1     # newValue <- rv$value - 1
    value(newValue)  }           # rv$value <- newValue
  })
  
  observeEvent(input$plus, {
    newValue <- value() + 1     # newValue <- rv$value + 1
    value(newValue)             # rv$value <- newValue
  })
  
  
#maybe better using insertUI https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  output$moreSubstanzen <- renderUI({
    lapply (1:value(), (function(i){
      # input[[paste0('moreSubstanzen', i)]] <- 
      selectizeInput(paste0("Substanz", i), paste0("Substanz ", i),choices = Rezeptursammlung$V2)
    }))
  })
  
  

  
  # Rezeptursammlung_sub <- eventReactive(input$Juniormed,{
  #   a <- Rezeptursammlung[which(Rezeptursammlung$V2 == input$Substanz1),]$V1
  #   
  #   
  #   Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% a)
  #   
  #   c <- Rezeptursammlung[which(Rezeptursammlung$V2 == input$Substanz2),]$V1
  #   
  #   Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% c)
  #   Rezeptursammlung
  #   
  # })
  
  Rezeptursammlung_sub <- eventReactive(input$Juniormed,{
    
    for (i in 1:value()){
      
      Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == input[[paste0('Substanz', i)]]),]$V1)
    }
 #   lapply (value(), (function(i){
    
    #paste0("a", i) <- Rezeptursammlung[which(Rezeptursammlung$V2 == input[[paste0('Substanz', i)]]),]$V1


  #  Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == input[[paste0('Substanz', i)]]),]$V1)


    Rezeptursammlung
})
  
  
  output$Rezepturen <- renderUI({
    Rezeptursammlung <- Rezeptursammlung_sub()
    Rezepturen <- unique(Rezeptursammlung$V1)
  #  Rezepturen
  #  Rezepturen <- Rezeptursammlung_sub()
    lapply(1:length(Rezeptursammlung_sub()), function(i){
        numlines <- which(Rezeptursammlung$V1 == Rezepturen[i])
        Bestandteile <- Rezeptursammlung$V2[numlines]
        
        tagList(
          
          #sapply(Bestandteile, function(j) as.character(tags$p(j)))
          actionButton(paste0("Rezeptur",i),HTML(paste0("<h3>",Rezepturen[i]),"</h3>", "<br/>", Bestandteile)))
        
      })
      
    
    
    
    # lapply(1:10, function(i) {
    #   output[[paste0('b', i)]] <- renderUI({
    #     strong(paste0('Hi, this is output B#', i))
    #   })
    
  })
  
 #  output$text <- renderText({
 #    sammlung <- Rezeptursammlung_sub()
 #    uni <- unique(sammlung$V1)
 #    num <- length(uni)
 #    uni[num]
 # #   renderUI({
 #     lapply (1:num, (function(i){
 #        renderTable(sammlung[uni[i],])
 #    
 #        renderUI ({tableOutput(paste0("Rezeptur", i))
 #        })
 #      }))
 #      
 #    })
    
  
  
  # output$moretab <- renderUI({
  #   lapply (1:value(), (function(i){
  #     # input[[paste0('moreSubstanzen', i)]] <- 
  #     selectizeInput(paste0("Substanz", i), paste0("Substanz ", i),choices = Rezeptursammlung$V2)
  #   }))
  # })
  # 

  
  # output$table <- renderTable({
  #   Rezepturen <- Rezeptursammlung_sub()
  #   num <- length(Rezepturen)
  #   print(num)
  #   for (i in 1:num){
  #   }
  #     })
  
#  output$moreSubstanzen <- renderUI({
#    lapply (1:value(), (function(i){
#      # input[[paste0('moreSubstanzen', i)]] <- 
#      selectizeInput(paste0("Substanz", i), paste0("Substanz ", i),choices = Rezeptursammlung$V2)
#    }))
#  })
  

  
  
  
  #updateSelectizeInput(session, "moreSubstanzen", choices = Rezeptursammlung$V2, server = TRUE)
    
}


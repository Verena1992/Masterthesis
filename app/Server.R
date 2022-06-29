library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
library(shinyFiles)
library(shinyjs)

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
Rezeptursammlung <- read.csv("~/Rezeptursammlung.txt", header=FALSE, sep=";")
juniormed_pagenr <- readRDS("~/data/Juniormed/juniormed_pagenr.rds")

Wirkstoff <- c()
Verdrängungsfaktor <- c()

server <- function(input, output, session) {

# Rezeptur hinzufügen--------------------------------------------------------------------------------------------- 
  
  new_Rezeptur <- eventReactive(input$eigeneRezeptur_hinzu,{
    
    content <- c(
    input$Titel, input$Herstellungshinweise, input$Quelle, input$Dosierung, 
    input$Haltbarkeit, input$Lagerung, input$Anwendung)
    
    titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung", 
               "Haltbarkeit", "Lagerung", "Anwendung")
    
    new_Rezeptur <- data.frame(content)
    new_Rezeptur <- t(new_Rezeptur)
    colnames(new_Rezeptur) <- titel
    new_Rezeptur
    #new_Rezeptur <- data.frame(titel, content)
    #new_Rezeptur <- t(new_Rezeptur)
    #browser()
  })
  
  output$new_Rezeptur <- renderTable({
    t(new_Rezeptur())
  }) 
  
  output$download_newRezeptur <- downloadHandler(
    filename = function() {
      paste0("Herstellungshinweise")
    },
    content = function(file) {
      new_Rezeptur <- new_Rezeptur()
      interne_Herstellungshinweise <- interne_Herstellungshinweise()
      titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung", 
                 "Haltbarkeit", "Lagerung", "Anwendung")
      colnames(interne_Herstellungshinweise) <- titel
      
      
      data_Verdrän <- rbind(new_Rezeptur,interne_Herstellungshinweise[-1,])
      
      vroom::vroom_write(data_Verdrän, 
                         file, delim = ";")
    }
  )
  
  
  
# Home-----------------------------------------------------
#upload and generate dataset
  
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
      dataSet
    }
  })
  
  interne_Rezeptursammlung <- reactive({
    req(input$file)
    file_list <- unzip("~/interne_Rezeptursammlung.zip", list = T, exdir = getwd())
    interne_Rezeptursammlung <- read.table(file_list[1,1], header=T, sep = "\t")
    interne_Rezeptursammlung
  })
  
   interne_Herstellungshinweise <- reactive({
     req(input$file)
     file_list <- unzip("~/interne_Rezeptursammlung.zip", list = T, exdir = getwd())
     interne_Herstellungshinweise <- read.table(file_list[2,1], header=F, sep = ";")
     interne_Herstellungshinweise
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

   value <- reactiveVal(1) 
   value_2 <- reactiveVal(1)
   
   Rezeptur <- reactiveVal() 
   Rezeptur2 <- reactiveVal()
   
   #number of selected interne Rezeptur, reactiveValues = False to use it with req(), 
   #tableOutput should wait until Rezeptur is selected -- if not error 
   selected_int_Rezeptur <- reactiveValues(num = FALSE)
   
  observeEvent(input$minus, {
    if (value() > 1) {
    newValue <- value() - 1     
    value(newValue)  }           
  })
  
  observeEvent(input$eR_minus, {
    if (value_2() > 1) {
      newValue <- value_2() - 1     
      value_2(newValue)  }           
  })
  
  observeEvent(input$plus, {
    newValue <- value() + 1     
    value(newValue)             
  })
  
  observeEvent(input$eR_plus, {
    newValue <- value_2() + 1     
    value_2(newValue)             
  })
  
    
#maybe better using insertUI https://shiny.rstudio.com/reference/shiny/1.0.3/observeEvent.html
  
  output$moreSubstanzen <- renderUI({
    lapply (1:value(), (function(i){
      # input[[paste0('moreSubstanzen', i)]] <- 
      selectizeInput(paste0("Substanz", i), paste0("Substanz ", i),choices = Rezeptursammlung$V2)
    }))
  })
  
  output$moreSubstanzen_2 <- renderUI({
    lapply (1:value_2(), (function(i){
      # input[[paste0('moreSubstanzen', i)]] <- 
      selectizeInput(paste0("Substanz_int", i), paste0("Substanz ", i),choices =  interne_Rezeptursammlung()$V2)
    }))
  })
    

  

  
  Rezeptursammlung_sub <- eventReactive(input$Juniormed,{
    
    for (i in 1:value()){
      
      Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == input[[paste0('Substanz', i)]]),]$V1)
    }
    Rezeptursammlung
})
  
  interne_Rezeptursammlung_sub <- eventReactive(input$ei_Rezeptur_B,{
    
    for (i in 1:value_2()){
      
      intRez <- interne_Rezeptursammlung()
      interne_Rezeptursammlung <- subset(intRez, V1 %in% intRez[which(intRez$V2 == input[[paste0('Substanz_int', i)]]),]$V1)
    }
    interne_Rezeptursammlung
  })
  
  
  
  
  
  output$Rezepturen <- renderUI({
    Rezeptursammlung <- Rezeptursammlung_sub()
    Rezepturen <- unique(Rezeptursammlung$V1)
    Rezeptur(Rezepturen)
  #  Rezepturen
  #  Rezepturen <- Rezeptursammlung_sub()
  #  1:nrow(Rezeptursammlung_sub()
    lapply(1:length(Rezepturen), function(i){
        numlines <- which(Rezeptursammlung$V1 == Rezepturen[i])
        Bestandteile <- Rezeptursammlung$V2[numlines]
        
        tagList(
          
          #sapply(Bestandteile, function(j) as.character(tags$p(j)))
          actionButton(paste0("Rezeptur",i),HTML(paste0("<h3>",Rezepturen[i]),"</h3>", "<br/>", Bestandteile)))
        
      })
  })
  
  
  
  output$Rezepturen_int <- renderUI({
    Rezeptursammlung_int <- interne_Rezeptursammlung_sub()
    Rezepturen_int <- unique(Rezeptursammlung_int$V1)
    Rezeptur2(Rezepturen)
    #  Rezepturen
    #  Rezepturen <- Rezeptursammlung_sub()
    #  1:nrow(Rezeptursammlung_sub()
    lapply(1:length(Rezepturen_int), function(i){
      numlines_int <- which(Rezeptursammlung_int$V1 == Rezepturen_int[i])
      Bestandteile_int <- Rezeptursammlung_int$V2[numlines_int]
      
      tagList(
        
        #sapply(Bestandteile, function(j) as.character(tags$p(j)))
        actionButton(paste0("Rezeptur_int",i),HTML(paste0("<h3>",Rezepturen_int[i]),"</h3>", "<br/>", Bestandteile_int)))
      
    })
  })
    
    
      # output$Herstellungshinweis <- renderUI({
      #   for (i in 1:10){
      #   if (input[[paste0('Rezeptur', i)]]){
      #   tags$iframe(src="http://juniormed.at/pdf/#kompendium/5", height=500, width=800)}}
      # })
      
      lapply(
        X = 1:10,
        FUN = function(i){
          observeEvent(input[[paste0("Rezeptur", i)]], {
            #JUN <- sub(".*JUN", "JUN", Rezepturen)
            JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
            print(JUN)
            src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
            print(src)
            #print(Rezeptur()[i])
            output$Herstellungshinweis <- renderUI({
              tags$iframe(src=src, height=500, width=800 )
             # tags$iframe(src=juniormed_pagenr$unlist.url_JUN.[i], height=500, width=800 )
           # tags$iframe(src="http://juniormed.at/pdf/#kompendium/5", height=500, width=800)
            })
          })
        }
      )
      
      lapply(
        X = 1:10,
        FUN = function(i){
          observeEvent(input[[paste0("Rezeptur_int", i)]], {
            #JUN <- sub(".*JUN", "JUN", Rezepturen)
            JUN_int <-  Rezeptur2()[i]
            print(JUN_int)
            
            interne_Herstellungshinweise <- interne_Herstellungshinweise()
            #browser()
            #print(interne_Herstellungshinweise$Titel)
            number <- which(interne_Herstellungshinweise$V1 == JUN_int)
            print(number)
            selected_int_Rezeptur$num <- number
            #src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
            #print(src)
            #print(Rezeptur()[i])
            
            #  tags$iframe(src=src, height=500, width=800 )
              # tags$iframe(src=juniormed_pagenr$unlist.url_JUN.[i], height=500, width=800 )
              # tags$iframe(src="http://juniormed.at/pdf/#kompendium/5", height=500, width=800)
            })
        #})
        }
      )
      
      table_int_sel_rezeptursammlung <- reactive({
        number <- selected_int_Rezeptur$num
        #req(selected_int_Rezeptur$state)
        a <- as.data.frame(t(interne_Herstellungshinweise()[c(1,number),]))
        req(selected_int_Rezeptur$num)
        colnames(a) <- c(unlist(interne_Herstellungshinweise()[1]))
        b <- a[-1,]
        b
      })
      
      
        
      
      output$Herstellungstext_int <- renderTable(
        
        #interne_Herstellungshinweise <- interne_Herstellungshinweise()
        #as.data.frame(t(starting_df))
        table_int_sel_rezeptursammlung()
        
      )
      
      
      
      
      # interne_Rezeptursammlung <- reactive({
      #   req(input$interne_Rezeptursammlung)#to make sure code waits until the first file is uploaded
      #   #first column = c(character), second = double
      #   #verdrängungsfaktor needs to be written with point as comma
      #   #datapath = The path to a temp file that contains the data that was uploaded
      #   #ext <- tools::file_ext(input$Verdrängungsfaktoren$datapath)
      #   #validate(need(ext == "txt" | ext == "pdf", "Please upload a csv file"))
      #   
      #     dataSet <- vroom::vroom(input$interne_Rezeptursammlung$datapath, delim = "\t",col_types = "cc")
      #     
      #   dataSet
      # })
      
      output$interne_Rezeptursammlung <- reactive({
        return(!is.null(interne_Rezeptursammlung()))
      })
      outputOptions(output, 'interne_Rezeptursammlung', suspendWhenHidden=FALSE)
      
      
      #jump to new page
      observeEvent(input$jumpto_neueRezep, {
        updateTabsetPanel(session, "inTabset",
                          selected = "Rezepturhinzufügen")
      })
     
      output$zipped <-renderTable({
        req(input$file$datapath)
        list <- unzip(input$file$datapath, list = TRUE, exdir = getwd())
        int_rezep <- unz(list[1])
        browser()
      })
}


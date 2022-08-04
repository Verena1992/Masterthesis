#--------------------------------------------------------------------------
load_libraries()

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
#--------------------------------------------------------------------------

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")


Wirkstoff <- c()
Verdrängungsfaktor <- c()



server <- function(input, output, session) {

  
# Home-----------------------------------------------------

  
  #1 uploads
  #1.1. interne Sammlung
  
  rz <- createRezeptursammlungServer("jun_and_int")
  
  #1.1.1. Rezeptursammlung Zusammensetzung
  interne_Rezeptursammlung <- reactive({
    dataSet <- zip2dataSet(rz$datapath(), filenr = 1)
    dataSet
  })
  
  #1.1.2. Herstellungshinweise
  interne_Herstellungshinweise <- reactive({
    dataSet <- zip2dataSet(rz$datapath(), filenr = 2, header=F, sep = ";")
    dataSet
  })
  
  #1.1.3. Verdrängungsfaktoren
  data_Verdrän <- reactive({
    #if interne Sammlung is uploaded and nrf merge both
      verdrängungsfaktoren <- createVerdrängungsfaktorenServer("nrf_and_int", rz$datapath())
  })
  

  #2.output added information by user
  
  #2.1.Herstellungshinweis
  output$new_Herstellungshinweis <- renderTable({
    t(new_Herstellungshinweis())
  }) 
  
  #2.2.Verdrängungsfaktor
  output$new_Verdrängungsfaktor <- renderTable({
    new_data()
  })


  #3. Update
  
  #3.1.Rezeptursammlung Zusammensetzung
  updated_Rezeptur_Zusam <- reactive({
    new_Rezeptur_Zusam <- new_Rezeptur_Zusam$Substanzen()
    interne_Rezeptursammlung <- interne_Rezeptursammlung()
    titel <- rep(new_Herstellungshinweis()[1], length(new_Rezeptur_Zusam))
    new_Rezeptur_Zusam_df <- cbind(titel, new_Rezeptur_Zusam)
    colnames(new_Rezeptur_Zusam_df) <- c("V1", "V2")
    updated_Rezeptur_Zusam <- rbind(interne_Rezeptursammlung, new_Rezeptur_Zusam_df)
  })
  
  #3.2.Herstellungshinweise
  updated_Herstellungshinweise <- reactive({
    new_Herstellungshinweis <- new_Herstellungshinweis()
    interne_Herstellungshinweise <- interne_Herstellungshinweise()
    titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung",
               "Haltbarkeit", "Lagerung", "Anwendung")
    colnames(interne_Herstellungshinweise) <- titel

    updated_Herstellungshinweise <- rbind(new_Herstellungshinweis,interne_Herstellungshinweise[-1,])
    updated_Herstellungshinweise
  })
  
  #3.3.Verdrängungsfaktoren
  
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
  })

  
  updated_Verdrängungsfaktoren <- reactive({
    interne_Verdrängungsfaktoren <- data_Verdrän()
    data_Verdrän <- rbind(new_data(),interne_Verdrängungsfaktoren())
    data_Verdrän
  })
  

  
  
  
  
  #4 Download zip file
  
  output$download_newRezeptur <- downloadHandler(
    filename = 'interene_Rezepturen.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      
      
      fs <- c("Rezeptur_Zusa", "Herstellungshinweise", "Verdrängungsfaktoren")
      if (rezepturhinweiseServer("textAreas")[1] == ""){
       # browser()
        vroom::vroom_write(interne_Rezeptursammlung(), 
                           "Rezeptur_Zusa", delim = "\t")
        interne_Herstellungshinweise <- interne_Herstellungshinweise()
        titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung",
                   "Haltbarkeit", "Lagerung", "Anwendung")
        colnames(interne_Herstellungshinweise) <- titel
        vroom::vroom_write(interne_Herstellungshinweise[-1,], 
                           "Herstellungshinweise", delim = ";")
      } else {
       # browser()
        vroom::vroom_write(updated_Rezeptur_Zusam(), 
                           "Rezeptur_Zusa", delim = "\t")
        vroom::vroom_write(updated_Herstellungshinweise(), 
                           "Herstellungshinweise", delim = ";")
        }
      if (input$Substanz_hinzufügen | input$Substanz_hinzufügen2 | input$Substanz_hinzufügen3){
      vroom::vroom_write(updated_Verdrängungsfaktoren(), 
                         "Verdrängungsfaktoren", delim = "\t")
      } else {
        interne_Verdrängungsfaktoren <- data_Verdrän()
        vroom::vroom_write(interne_Verdrängungsfaktoren(), 
                           "Verdrängungsfaktoren", delim = "\t")
      }
      
      zip(zipfile=fname, files=fs)
      if(file.exists(paste0(fname, ".zip"))) {file.rename(paste0(fname, ".zip"), fname)}
    },
    contentType = "application/zip"
  ) 
  
  
  
  


# Rezeptursammlung----------------------------------------------------------   
  
  
  
  output$selectizeInput01 <- renderUI({
    
    selectizeInput("zusammensetzungRezep", "Zusammensetzung der Rezeptur",choices = rz$rezeptursammlung()$V2, multiple = TRUE,
                   options = list(placeholder = "wähle Substanzen aus"))
  })
  
  
  Bestandteile <- reactive({
    if(!is.null(input$zusammensetzungRezep)){
     # browser()
    Bestandteile_ex <- foundRezepturenButtonServer("button",input$zusammensetzungRezep, rz$rezeptursammlung(), rz$datapath())
    }
  })
 
  
    output$erstattungscheck <- renderUI({
      Bestandteile <- Bestandteile()
      
      if (!is.null(Bestandteile())){
      #browser()
      #  actionButton("erstattungsfähigkeit", "Erstattungsfähigkeit der ausgewählten Rezeptur prüfen")}
      big_yellow_button("erstattungsfähigkeit", "Erstattungsfähigkeit der ausgewählten Rezeptur prüfen")}
    })
  
    observeEvent(input$erstattungsfähigkeit, {
      updateTabsetPanel(session, "inTabset",
                        selected = "Erstattungscheck")
    })
  
#--------------------------------------------------------------------------
  
  

#neue_Zusammensetzung_Rezeptur----------------------------------------------------------------------------------------------------
  
  #show tab only if zip file is uploaded
  hideTab("inTabset", "neue_Zusammensetzung_Rezeptur")
  observeEvent(rz$datapath(), {
    showTab("inTabset", "neue_Zusammensetzung_Rezeptur");
  })

  
  #return: Substanzen -- (new_Rezeptur_Zusam$Substanzen()) 
  #        and if button jump to Herstellungshinweise is clicked
  new_Rezeptur_Zusam <- addRezepturServer("Zusammensetzung", taxe_eko)
  
#--------------------------------------------------------------------------
   
  
#Rezeptur hinzufügen--------------------------------------------------------------------------------------------- 
  
  #show tab only if zip file is uploaded
  hideTab("inTabset", "Rezepturhinzufügen")
  observeEvent(rz$datapath(), {
    showTab("inTabset", "Rezepturhinzufügen")
  })
  
  #if yellow button in rz-zusammensetzung is clicked jump to this panel
  observeEvent(new_Rezeptur_Zusam$jump_to_Herstellungshinweise(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "Rezepturhinzufügen")
  })
  

  new_Herstellungshinweis <- eventReactive(input$eigeneRezeptur_hinzu,{
    rezepturhinweiseServer("textAreas")
  })
#--------------------------------------------------------------------------


  
  
# Erstattungscheck---------------------------------------------------
  
  observe({
    Bestandteile <- Bestandteile()
    
    if (!is.null(Bestandteile())){
  
    erstattungscheckServer("ec", taxe_eko, Bestandteile())
    } else {
      
      
      erstattungscheckServer("ec", taxe_eko)
    }
    })
  
  
  
#--------------------------------------------------------------------------
  
  
  
  
  
  observe({
    #req(rz$datapath())
    dataSet <- data_Verdrän()
    if(!is.null(dataSet())){
    updateSelectizeInput(session, "WS_S", choices = c(dataSet()$Wirkstoff, "Substanz nicht in Liste vorhanden"))
    }
  })
  
  observe({
    #req(rz$datapath())
    dataSet <- data_Verdrän()
    if(!is.null(dataSet())){
    updateSelectizeInput(session, "WS_S2", choices = c(dataSet()$Wirkstoff, "Substanz nicht in Liste vorhanden"))
    }
  })
  
  observe({
    #req(rz$datapath())
    dataSet <- data_Verdrän()
    if(!is.null(dataSet())){
    updateSelectizeInput(session, "WS_S3", choices = c(dataSet()$Wirkstoff, "Substanz nicht in Liste vorhanden"))
    }
  })
  
  
  
  output$fileUploaded <- reactive({
    dataSet <- data_Verdrän()
    return(!is.null(dataSet()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  vf1 <- reactive({
    dataSet <- data_Verdrän()
    vf <- find_verdrängungsf(input$WS_S, input$Substanz_hinzufügen, input$New_Verdrängungsfaktor, dataSet())
  })
  
  vf2 <- reactive({
    dataSet <- data_Verdrän()
    vf <- find_verdrängungsf(input$WS_S2, input$Substanz_hinzufügen2, input$New_Verdrängungsfaktor2, dataSet())
  })

  vf3 <- reactive({
    dataSet <- data_Verdrän()
    vf <- find_verdrängungsf(input$WS_S3, input$Substanz_hinzufügen3, input$New_Verdrängungsfaktor3, dataSet())
  })
  
  output$vf <- renderUI({
    vf <- vf1()
    HTML(paste("Verdrägungsfaktor = ",vf))
  })
  
  output$vf2 <- renderUI({
    vf <- vf2()
    HTML(paste("Verdrägungsfaktor = ",vf))
  })
  
  output$vf3 <- renderUI({
    vf <- vf3()
    HTML(paste("Verdrägungsfaktor = ",vf))
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
    dataSet <- data_Verdrän()
    if(is.null(dataSet())){
    
    verdrängung1 <- (input$Menge_Substanz1 * input$Stückanzahl * input$Vf)
    verdrängung2 <- if(input$weitere_Substanz > input$Substanz2_entfernen){
      (input$Menge_Substanz2 * input$Stückanzahl * input$Vf)} else {0}
    verdrängung3 <- if(input$weitere_Substanz2 > input$Substanz3_entfernen){
      (input$Menge_Substanz3 * input$Stückanzahl * input$Vf)} else {0}
    verdrängung <- verdrängung1 + verdrängung2 + verdrängung3
    } else {
      #browser()
      verdrängung1 <- (input$Menge_Substanz1 * input$Stückanzahl * as.numeric(vf1()))
      verdrängung2 <- if(input$weitere_Substanz > input$Substanz2_entfernen){
        (input$Menge_Substanz2 * input$Stückanzahl * as.numeric(vf2()))} else {0}
      verdrängung3 <- if(input$weitere_Substanz2 > input$Substanz3_entfernen){
        (input$Menge_Substanz3 * input$Stückanzahl * as.numeric(vf2()))} else {0}
      verdrängung <- verdrängung1 + verdrängung2 + verdrängung3  
      
      
      
    }
    einwaage_ohne_WS <- input$Stückanzahl * input$Eichwert           
                  
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
  
# Rezeptpflicht---------------------------------------------------------------------------- 
  
  updateSelectizeInput(session, "WS", choices = rezeptpflicht$Wirkstoff, server = TRUE
  )
  
  output$Rstatus <- renderPrint({
    selected_ws <- rezeptpflicht[which(rezeptpflicht$Wirkstoff == input$WS),]
    Rstatus <- selected_ws$Rstatus
    Rstatus
  })
#-------------------------------------------------------------------------------------------

     

}


#--------------------------------------------------------------------------
load_libraries()
#--------------------------------------------------------------------------

rezeptpflicht <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")


Wirkstoff <- c()
Verdrängungsfaktor <- c()



server <- function(input, output, session) {
  


# Rezeptursammlung----------------------------------------------------------   
  
  rz <- createRezeptursammlungServer("jun_and_int")
  
  output$selectizeInput01 <- renderUI({
    
    selectizeInput("zusammensetzungRezep", "Zusammensetzung der Rezeptur",choices = rz$rezeptursammlung()$V2, multiple = TRUE,
                   options = list(placeholder = "wähle Substanzen aus"))
  })
  
  observeEvent(input$zusammensetzungRezep,{
    foundRezepturenButtonServer("button",input$zusammensetzungRezep, rz$rezeptursammlung(), rz$datapath())
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
  
  #output$text2 <- renderTable(
  #  new_Rezeptur_Zusam$Substanzen())
  
  
   
# Rezeptur hinzufügen2---------------------------------------------------------------------------------------------  

   observeEvent(new_Rezeptur_Zusam$jump_to_Herstellungshinweise(), {
     updateTabsetPanel(session, "inTabset",
                       selected = "Rezepturhinzufügen")
   })
  # 
  # Rezepturzusammensetzung_server
  
# Rezeptur hinzufügen--------------------------------------------------------------------------------------------- 
  
  hideTab("inTabset", "Rezepturhinzufügen")
  
  observeEvent(rz$datapath(), {
    showTab("inTabset", "Rezepturhinzufügen")
  })
  
  
  new_Rezeptur <- eventReactive(input$eigeneRezeptur_hinzu,{
    rezepturhinweiseServer("textAreas")
  })
  
  output$new_Rezeptur <- renderTable({
    t(new_Rezeptur())
  }) 
  
  
  updated_Herstellungshinweise <- reactive({
    new_Rezeptur <- new_Rezeptur()
    interne_Herstellungshinweise <- interne_Herstellungshinweise()
    titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung", 
               "Haltbarkeit", "Lagerung", "Anwendung")
    colnames(interne_Herstellungshinweise) <- titel

    updated_Herstellungshinweise <- rbind(new_Rezeptur,interne_Herstellungshinweise[-1,])
    updated_Herstellungshinweise
  })
  
  
  output$download_newRezeptur <- downloadHandler(
    #https://www.reddit.com/r/rprogramming/comments/f53c59/zip_multiple_csvs_for_download/
    filename = function() {
      paste0("Herstellungshinweise")
    },
    content = function(file) {

      
      vroom::vroom_write(updated_Herstellungshinweise(), 
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

      #jump to new page
      observeEvent(input$jumpto_neueRezep, {
        updateTabsetPanel(session, "inTabset",
                          selected = "neue_Zusammensetzung_Rezeptur")
      })
     

}


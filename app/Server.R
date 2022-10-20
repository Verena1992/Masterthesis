
# load libraries ----------------------------------------------------------
library(shiny)
library(readr)
library(vroom)
library(dplyr)
library(shinyalert)
library(auth0)
library(shinyWidgets)
library(shinyjs)
library(purrr)
library(shinyBS)
library(pdftools)
library(shinytest)
#library(knitr)

# load data ---------------------------------------------------------------

rezeptpflichtDS <- readRDS("./data/Rezeptpflicht/rezeptpflicht.rds")
#eclude one substance with a open bracket in the name
rezeptpflichtDS <- rezeptpflichtDS[-1605,]
taxe_eko <- readRDS("./data/Arzneitaxe/Arzneitaxe_eko.rds")
dosierung_lokal <- read.delim2("./data/NRF/Dosierung der Wirkstoffe zur Lokalanwendung.txt", header=FALSE)
bedenkliche_St <- read.delim("./data/bedenkliche_Substanzen/bedenkliche_St.txt")

# generate empty vectors --------------------------------------------------
Wirkstoff <- c()
Verdrängungsfaktor <- c()

# server------------------------------------------------------------------
#server <- auth0_server(function(input, output, session) {
server <- function(input, output, session) {

# Home-----------------------------------------------------

  
  #1 uploads
  #1.1. interne Sammlung
  #1.1.1. Rezeptursammlung Zusammensetzung +   #1.1.2. Herstellungshinweise
  rz <- createRezeptursammlungServer("jun_and_int")
  
  
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
  #  browser()
    new_Rezeptur_Zusam <- new_Rezeptur_Zusam$Substanzen()
    interne_Rezeptursammlung <- rz$interne_Rezeptursammlung()[,-3]
   # interne_Rezeptursammlung <- interne_Rezeptursammlung()
    titel <- rep(new_Herstellungshinweis()[1], length(new_Rezeptur_Zusam))
    new_Rezeptur_Zusam_df <- cbind(titel, new_Rezeptur_Zusam)
    colnames(new_Rezeptur_Zusam_df) <- c("V1", "V2")
    updated_Rezeptur_Zusam <- rbind(interne_Rezeptursammlung, new_Rezeptur_Zusam_df)
  })
  
  
  #3.2.Herstellungshinweise
  updated_Herstellungshinweise <- reactive({
    new_Herstellungshinweis <- new_Herstellungshinweis()
    interne_Herstellungshinweise <- rz$interne_Herstellungshinweise()
    #interne_Herstellungshinweise <- interne_Herstellungshinweise()
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
      interne_Herstellungshinweise <- rz$interne_Herstellungshinweise()
      titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung",
                 "Haltbarkeit", "Lagerung", "Anwendung")
      colnames(interne_Herstellungshinweise) <- titel
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      #setwd(tempdir())
      
      
      fs <- c("Rezeptur_Zusa", "Herstellungshinweise", "Verdrängungsfaktoren")
      if (rezepturhinweiseServer("textAreas")[1] == ""){
       
      #  vroom::vroom_write(interne_Rezeptursammlung(), 
       #                    "Rezeptur_Zusa", delim = "\t")
        vroom::vroom_write(rz$interne_Rezeptursammlung()[,-3], 
                           "Rezeptur_Zusa", delim = "\t")
        
      
        vroom::vroom_write(interne_Herstellungshinweise[-1,], 
       # vroom::vroom_write(interne_Herstellungshinweise[-1,], 
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
  
  
   observe({
     data <- sort(c(rz$rezeptursammlung()$V2,taxe_eko$wirkstoffe_arzneitaxe))
    updateSelectizeInput(session, inputId = "zusammensetzungRezep", choices = data ,server = TRUE
                       )
   })
  
  r <- reactiveValues(found = 1)
  
  observeEvent(input$rezep_nicht_gefun, {
    hide("rezep_nicht_gefun")
    r$found <- 0
  })
  
  observeEvent(input$reset_ec, {
    show("rezep_nicht_gefun")
    r$found <- 1
  })
  
  
  Bestandteile <- reactive({
    if(r$found == 0){
      Bestandteile_ex <- input$zusammensetzungRezep
    } else {
    Bestandteile_ex <- foundRezepturenButtonServer("button",input$zusammensetzungRezep, rz$rezeptursammlung(), rz$datapath())}
  })
    
    erstattungsstatus <- reactive({
      Bestandteile <- Bestandteile()
      box <- taxe_eko[taxe_eko$wirkstoffe_arzneitaxe %in% Bestandteile(),]$box
    })
  
    output$erstattungscheck <- renderUI({
      Bestandteile <- Bestandteile()
      if (!is.null(Bestandteile())){
      box <- erstattungsstatus()
        if(all(box == "grün") & (length(box) == length(Bestandteile()))){
          tagList(
          tags$hr(),
          big_green_button("erstattungsfähigkeit", "Erstattungsfähigkeit der ausgewählten Rezeptur prüfen"))
        } else {
          tagList(
          tags$hr(),
          big_red_button("erstattungsfähigkeit", "Achtung! Kontrolliere Erstattungsfähigkeit der ausgewählten Rezeptur"))
        }
      }
    })
    
 
    
    output$hartfettberechnen <- renderUI({
      Bestandteile <- Bestandteile()
      if (!is.null(Bestandteile())){
        
        if(is.element("Hartfett", Bestandteile())){
          tagList(
            tags$hr(),
          big_yellow_button("hartfettberechner", "Hartfettmenge der ausgewählten Rezeptur berechnen"))
         
        }
        
      }
    })
    
  
      
        output$bedenklicher_Stoff <- renderUI({
          if (!is_empty(bs$bedenkliche_Substanz())){
            tagList(
              tags$hr(),
            big_red_button("bedenklich", "ein bedenklicher Stoff wurde eingegeben!!"))
          }
        })
      
    
   
    observeEvent(input$erstattungsfähigkeit, {
      changePanel(session, "erstattungsfähigkeit")
    })    
        
    observeEvent(input$hartfettberechner, {
      changePanel(session, "Hartfettmengenrechner")
    })
    
    observeEvent(input$kompatibilitätscheck, {
      changePanel(session, "Kombatibilitätscheck")
    })
    
    observeEvent(input$bedenklich, {
      changePanel(session, "bedenkliche Stoffe")
    })
    
    observeEvent(input$dosierung, {
      changePanel(session, "Dosierungscheck")
    })
    
    observeEvent(input$bedenkliche_RZ, {
      changePanel(session, "bedenkliche Stoffe")
    })
    
    observeEvent(input$rezeptpflicht, {
      changePanel(session, "Rezeptpflichtcheck")
    })
    
    
    
    observeEvent(input$reset_ec,{
      reset("zusammensetzungRezep")
      
    })
  

    
# neue_Zusammensetzung_Rezeptur----------------------------------------------------------------------------------------------------
  
  #show tab only if zip file is uploaded
  #hide("download_newRezeptur")
  hideTab("inTabset", "neue_Zusammensetzung_Rezeptur")
  observeEvent(rz$datapath(), {
    showTab("inTabset", "neue_Zusammensetzung_Rezeptur");
   # show("download_newRezeptur")
  })

  
  #return: Substanzen -- (new_Rezeptur_Zusam$Substanzen()) 
  #        and if button jump to Herstellungshinweise is clicked
  new_Rezeptur_Zusam <- addRezepturServer("Zusammensetzung", taxe_eko)
  

# Rezeptur hinzufügen--------------------------------------------------------------------------------------------- 
  
  output$download <- renderUI({
    tagList(
    tags$h2("Downloads"),
    tags$h5("Neue Informationen zur interner Sammlung hinzufügen und herunterladen"),
    downloadButton("download_newRezeptur", label = "Download"),
    tags$hr())
  })
  
  #show tab only if zip file is uploaded
  hide("download")
  hideTab("inTabset", "Rezepturhinzufügen")
  observeEvent(rz$datapath(), {
    showTab("inTabset", "Rezepturhinzufügen")
    show("download")
  })
  
  #if yellow button in rz-zusammensetzung is clicked jump to this panel
  observeEvent(new_Rezeptur_Zusam$jump_to_Herstellungshinweise(), {
    updateTabsetPanel(session, "inTabset",
                      selected = "Rezepturhinzufügen")
  })
  

  new_Herstellungshinweis <- eventReactive(input$eigeneRezeptur_hinzu,{
    rezepturhinweiseServer("textAreas")
  })

# Erstattungscheck---------------------------------------------------
  
  observe({
    Bestandteile <- Bestandteile()
    
     # erstattungscheckServer("ec", taxe_eko, Bestandteile())
      
      esc <-  erstattungscheckServer("ec", taxe_eko, Bestandteile())
      output$enf <- renderText({
        esc$element_not_found()
      })
      #https://github.com/rstudio/shiny/issues/1318
      outputOptions(output, "enf", suspendWhenHidden = FALSE)

      output$eng <- renderText({
        esc$not_green()
      })
      outputOptions(output, "eng", suspendWhenHidden = FALSE)
  })
  


  
  #esc <-  erstattungscheckServer("ec", taxe_eko, Bestandteile())
  

  

# Kompatibilitätscheck---------------------------------------------------------
  
  observe({
    Bestandteile <- Bestandteile()
    kc <- kompatibilitätscheckServer("Salbenfibel", Bestandteile())
    
    output$kompatibilität <- renderUI({
      Bestandteile <- Bestandteile()
      if (!is.null(Bestandteile())){
        Salbengrundlage <- c("Ultrasicc (R)", "Ultraphil (R)", "Ultrabas (R)", "Ultralip (R)")
        if(!is_empty(intersect(Salbengrundlage, Bestandteile()))){
         # browser()
          if(!is_empty(kc$element_not_kompatibel())){
            tagList(
              tags$hr(),
            big_red_button("kompatibilitätscheck", "Kompatibilität der ausgewählten Rezeptur prüfen"))
          }
          else if (!is.null(kc$element_not_found())){
            tagList(
            tags$hr(),
            big_yellow_button("kompatibilitätscheck", "Kompatibilität der ausgewählten Rezeptur prüfen"))
            }
          
          else if (kc$kompatibel() == TRUE){
            tagList(
              tags$hr(),
            big_green_button("kompatibilitätscheck", "Kompatibilität der ausgewählten Rezeptur prüfen"))
          } else {
            tagList(
              tags$hr(),
            big_yellow_button("kompatibilitätscheck", "Kompatibilität der ausgewählten Rezeptur prüfen"))
          }
        }

      }
    })
    
    output$enf_kc <- renderText({
      kc$element_not_found()
    })
    
    outputOptions(output, "enf_kc", suspendWhenHidden = FALSE)
    
    
  })
  

  
  

  
  
# Hartfettmengenrechner --------------------------------------------------------------------------  
  observe({
    #Bestandteile <- Bestandteile()
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
      (input$Menge_Substanz2 * input$Stückanzahl * input$Vf2)} else {0} 
    verdrängung3 <- if(input$weitere_Substanz2 > input$Substanz3_entfernen){
      (input$Menge_Substanz3 * input$Stückanzahl * input$Vf3)} else {0}
    verdrängung <- verdrängung1 + verdrängung2 + verdrängung3
    } else {
      #browser()
      verdrängung1 <- (input$Menge_Substanz1 * input$Stückanzahl * as.numeric(vf1()))
      verdrängung2 <- if(input$weitere_Substanz > input$Substanz2_entfernen){
        (input$Menge_Substanz2 * input$Stückanzahl * as.numeric(vf2()))} else {0}
      verdrängung3 <- if(input$weitere_Substanz2 > input$Substanz3_entfernen){
        (input$Menge_Substanz3 * input$Stückanzahl * as.numeric(vf3()))} else {0}
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

  

# rezeptpflichtcheck---------------------------------------------------------------------------- 
  
  
  observe({
    Bestandteile <- Bestandteile()
    rpf <- rezeptpflichtServer("rezeptpflicht", rezeptpflichtDS, Bestandteile())
    
    output$rezeptpflicht <- renderUI({
      Bestandteile <- Bestandteile()
      if (!is.null(Bestandteile())){
        if (!is_empty(rpf$element_rpf())){
          tagList(
            tags$hr(),
            big_red_button("rezeptpflicht", "Rezeptpflicht"))
        }
      }
    })
    
  })
  

# bedenkliche Stoffe-------------------------------------------------------------------------
  



#https://stackoverflow.com/questions/54677043/unable-to-pass-user-inputs-into-r-shiny-modules
#when passing global user input to a shiny module. It seems it will "break" the reactivity. You can fix this my explicitly passing in a reactive object. 
bs <- bedenklichStServer("arzneimittelkommission", Rezepturzusammensetzung = reactive(input$zusammensetzungRezep), bedenkliche_St)
 
    output$bedenklicheSt <- renderUI({
    Bestandteile <- Bestandteile()
    if (!is.null(Bestandteile())){

    if(!is_empty(intersect(bedenkliche_St$Stoffe, Bestandteile()))){
      hide("bedenklicher_Stoff")
      bedenk <- intersect(bedenkliche_St$Stoffe, Bestandteile())
      if(length(bedenk) == 1){
        tagList(
          tags$hr(),
      big_red_button("bedenkliche_RZ", paste0("ausgewählte Rezeptur enthält ",bedenk," als bedenkliche Substanz")))
      } else {
        tagList(
          tags$hr(),
        big_red_button("bedenkliche_RZ", paste0("ausgewählte Rezeptur enthält mehrere bedenkliche Substanzen")))
      }
    }}
    })

  
  
  
  
# dosierung------------------------------------------------------------------------------
  
observe({
    Bestandteile <- Bestandteile()
    ds <- dosierungServer("dosierung", Bestandteile())
    
    #gelber Button wenn Dosierungsinformationen über ein Bestandteil vorhanden sind 
    output$dosierung <- renderUI({
      Bestandteile <- Bestandteile()
      if (!is.null(Bestandteile())){
        if(!is_empty(intersect(dosierung_lokal$V1, Bestandteile()))){
            tagList(
              tags$hr(),
            big_yellow_button("dosierung", "Dosierung der ausgewählten Rezeptur prüfen"))
        }
      }
    })
        
      output$limits <- renderText({
        req(ds$not_within_con())
        if(ds$not_within_con()){
          "Achtung!"
        }
      })
      
      outputOptions(output, "limits", suspendWhenHidden = FALSE)
    
})

    
    
}





#02/07/2022
#1. read in Rezeptursammlung, selected Substanzen and datapath from uploaded zipfolder
#2. subsetting Rezeptursammlung with selected Substanzen
#3. output Buttons with matched Rezepturen
#4. output Herstellungshinweis from by the user selected Rezeptur

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


#UI-----------------------------------------------------
foundRezepturenButtonUI <- function(id) {
   
  tagList(
    useShinyjs(),
    uiOutput(NS(id,"Rezepturen")),
    textOutput(NS(id, "keine_rez")),
    uiOutput(NS(id,"Herstellungshinweis")),
    tableOutput(NS(id,"Herstellungstext_int"))
  )
}


#Server-------------------------------------------------
foundRezepturenButtonServer <- function(id, Substanzen, Rezeptursammlung,datapathHH) {
  moduleServer(id, function(input, output, session) {
    
    Bestandteile_ex <- reactiveVal(NULL)
    
    #1.find Rezepturen
    Rezeptur <- reactiveVal(NULL)
    subRezepturSammlung <- reactive({subsettingRSammlung(Substanzen, Rezeptursammlung)})
    Rezepturen <- reactive({unique(subRezepturSammlung()$V1)})
    Rezeptur(Rezepturen())
    
    
    #1.1.if no Rezepturen are found, print a text:
    if (length(Rezeptur()) == 0 | length(Substanzen) == 0  ){
      show("keine_rez")
      output$keine_rez <- renderText("es wurde keine Rezeptur gefunden")
      hide("Herstellungshinweis")
      hide("Rezepturen")
      hide("Herstellungstext_int")
      Bestandteile_ex(NULL)
      
    } else {
      hide("keine_rez")
      show("Rezepturen")
    
    #1.2. for matched Rezepturen create a Button 
      output$Rezepturen <- renderUI({
        show("Rezepturen")
        hide("Herstellungshinweis")
        hide("Herstellungstext_int")
  
        ns <- session$ns
        
        lapply(1:length(Rezeptur()), function(i){
          numlines <- which(Rezeptursammlung$V1 == Rezeptur()[i])
          Bestandteile <- Rezeptursammlung$V2[numlines]
          if (unique(Rezeptursammlung$origin[numlines]) == 1)  {
          juniormed_pagenr <- readRDS("./data/Juniormed/juniormed_pagenr.rds")
          JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
          src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
            actionButton(ns(Rezeptur()[i]),HTML(paste0("<h3>",Rezeptur()[i]),"</h3>", "<br/>", Bestandteile), 
                         block = TRUE,
                         onclick = paste0("window.open('",src,"', '_blank')")
            )
          } else {
            actionButton(ns(Rezeptur()[i]), HTML(paste0("<h3>",Rezeptur()[i]),"</h3>", "<br/>", Bestandteile),
                 block = TRUE        
            )
          }
                        
        })
      }) 
        
      
      
      
      
        #1.2.1. observe if user clicks on a Rezepturbutton and print either Juniormed-page or Table
        lapply(1:length(Rezeptur()), function(i){
        
            observeEvent(input[[Rezeptur()[i]]], {
              
              numlines <- which(Rezeptursammlung$V1 == Rezeptur()[i])
              
              #is selected Rezeptur a Juniormed Rezeptur(1)?
              if ( unique(Rezeptursammlung$origin[numlines]) == 1)  {
                show("Herstellungshinweis")
                hide("Herstellungstext_int")
                hide("Rezepturen")
                Bestandteile_ex(Rezeptursammlung$V2[numlines])
                
                juniormed_pagenr <- readRDS("./data/Juniormed/juniormed_pagenr.rds")
                
                JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
               
                src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
              
                
                output$Herstellungshinweis <- renderUI({
                  tags$h3("Herstellungshinweis wird in einem neuem Fenster geöffnet..")
                })
                
                
              #is selected Rezeptur interne Rezeptur (2)?  
              } else if (unique(Rezeptursammlung$origin[numlines]) == 2){
                show("Herstellungstext_int")
                hide("Herstellungshinweis")
                hide("Rezepturen")
                Bestandteile_ex(Rezeptursammlung$V2[numlines])
                
                selected_int_Rezeptur <- reactiveValues(num = FALSE)
                
                #read in uploaded interne Herstellungshinweise
                interne_Herstellungshinweise <- reactive({
                  dataSet <- zip2dataSet(datapathHH, filenr = 2, header=F, sep = ";")
                  dataSet
                })
                
                
                int <-  Rezeptur()[i]
                interne_Herstellungshinweise <- interne_Herstellungshinweise()
                number <- which(interne_Herstellungshinweise$V1 == int)
                selected_int_Rezeptur$num <- number
        
                table_int_sel_rezeptursammlung <- reactive({
                      number <- selected_int_Rezeptur$num
                      a <- as.data.frame(t(interne_Herstellungshinweise[c(1,number),]))
                      req(selected_int_Rezeptur$num)
                      colnames(a) <- c("Titel", interne_Herstellungshinweise[["V1"]][number])
                      tab <- a[-1,]
                      tab
                })

                output$Herstellungstext_int <- renderTable(
                  table_int_sel_rezeptursammlung()
                )
                  
              } else {
                  print("upps")
              }
             }, autoDestroy = TRUE, ignoreNULL = T, ignoreInit = TRUE, once = TRUE)
          
          })
        
    }
    Bestandteile_ex
  })
}

#Test module:
# 
# foundRezepturenButtonApp <- function() {
#   ui <- fluidPage(
#     foundRezepturenButtonUI("button"),
#     #textOutput("text")
# 
#   )
# 
#   server <- function(input, output, session) {
#     Rezeptursammlung1 <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
#     Rezeptursammlung2 <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
#     Rezeptursammlung1 <- adorigin2dataframe(Rezeptursammlung1, 1)
#     Rezeptursammlung2 <- adorigin2dataframe(Rezeptursammlung2, 2)
#     Rezeptursammlung <- rbind(Rezeptursammlung2, Rezeptursammlung1)
#     Substanzen <- c("Atropinsulfat", "Natriumchlorid")
#   #  Substanzen <- c("Hartfett")
#     datapathHH <- c("interne_Rezeptursammlung.zip")
#     foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung, datapathHH)
#     #Rezepturen <- foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung)
#     #output$text <- renderText(foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung))
#   }
# 
#   shinyApp(ui, server)
# }
# # 
#  foundRezepturenButtonApp()










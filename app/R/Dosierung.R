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
library(DT)
library(ggplot2)

#---------------------------------------------------------------
dosierungUI <- function(id) {
  #ns <- NS(id)
  tagList(
    numericInput(NS(id,"Mengeinsgesamt"), "insgesamt Herzustellende Menge", value = 100, min = 0, max = 1000, step = 1),
    #https://github.com/rstudio/shiny/issues/1182
    selectizeInput(NS(id,"zusammensetzung_arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                   options = list(placeholder = "wähle Substanzen aus", maxItems = 1)), 
    numericInput(NS(id,"Menge_Substanz1"), "Menge der zu prüfende Substanz (g)", value = NULL, min = 0, max = 1, step = 0.01), 
    
    plotOutput(NS(id, "plot"), width = "53%", height = "100px"), 
    
    tableOutput(NS(id, "table"))
  )
}

dosierungServer <- function(id, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      dosierung_lokal <- read.delim2("./data/NRF/Dosierung der Wirkstoffe zur Lokalanwendung.txt", header=FALSE)
      updateSelectizeInput(session, inputId = 'zusammensetzung_arzneitaxe', choices = dosierung_lokal$V1, selected = Bestandteile, server = TRUE)
    
    
    calc_perce <- reactive({
     # browser()
      calc_perce <- (input$Menge_Substanz1/input$Mengeinsgesamt) * 100
      calc_perce
    })
    
    
    output$plot <- renderPlot({
      # df <- data.frame(id=LETTERS[1:4], min=c(13,15,23,2), max=c(20,30,40,11))
      # 
      # ggplot(df, aes(x=id))+
      #   geom_linerange(aes(ymin=min,ymax=max),linetype=2,color="blue")+
      #   geom_point(aes(y=min),size=3,color="red")+
      #   geom_point(aes(y=max),size=3,color="red")+
      #   theme_bw()
      
       ggplot(dosierung_lokal[which(dosierung_lokal$V1 == input$zusammensetzung_arzneitaxe),], aes(x=V1))+
      #  ggplot(dosierung_lokal[which(dosierung_lokal$V1 == "Erythromycin"),], aes(x=V1))+
        geom_linerange(aes(ymin=V3,ymax=V4),linetype=1,color="blue")+
        geom_point(aes(y=V3),shape=15, size=3,color="blue")+
        #geom_text()+
        geom_point(aes(y=V4),shape=15, size=3,color="blue")+
        geom_point(aes(y=calc_perce()),shape=8, size=5,color="red", na.rm = TRUE)+
        
        ylab("therapeutische Konzentration [%]")+
        ggtitle("therapeutische Konzentration [%]")+
        
        theme_bw()+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        
        theme(
          plot.title = element_text(size=16, face="bold"),
          #axis.title.x = element_text(size=15),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )+
       coord_flip()
       #geom_text(hjust=0, vjust=0)
      
      
    })
    
    not_within_con <- reactive({
      calc_perce()<dosierung_lokal[which(dosierung_lokal$V1 == input$zusammensetzung_arzneitaxe),]$V3 | calc_perce()>dosierung_lokal[which(dosierung_lokal$V1 == input$zusammensetzung_arzneitaxe),]$V4
    })
    
    output$table <- renderTable({
      #browser()
      df <- dosierung_lokal[which(dosierung_lokal$V1 == input$zusammensetzung_arzneitaxe),c(3,4)]
      df$V5 <- calc_perce()
      colnames(df) <- c("untere Grenze", "obere Grenze", "Konzentration Rezeptur")
      df
      #dosierung_lokal[which(dosierung_lokal$V1 == input$zusammensetzung_arzneitaxe),]$V3
      #browser()
    })
    
    list(
      calc_perce = reactive(calc_perce()), 
      not_within_con = reactive(not_within_con()))
    
    
    
  })
}


# #Test module:

# dosierungApp <- function() {
# ui <- fluidPage(
#   dosierungUI("dosierung"), 
#   
#   textOutput("perc"), 
#   textOutput("notwithin")
# )
# 
# server <- function(input, output, session) {
#   #Bestandteile <- c("Erythromycin")
#   Bestandteile <- c(NULL)
#   ds <- dosierungServer("dosierung", Bestandteile)
#   
#     output$perc <- renderText({
#       ds$calc_perce()
#     })
#     
#     output$notwithin <- renderText({
#       ds$not_within_con()
#     })
# 
# }
# 
# shinyApp(ui, server)}
# dosierungApp()
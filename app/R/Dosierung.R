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
library(DT)
library(ggplot2)

#UI-----------------------------------------------------
dosierungUI <- function(id) {
  tagList(
    fluidPage(
      fluidRow(
        column(3,
          wellPanel(
            numericInput(NS(id,"Mengeinsgesamt"), "insgesamt Herzustellende Menge", value = 100, min = 0, max = 1000, step = 1),
            #https://github.com/rstudio/shiny/issues/1182
            selectizeInput(NS(id,"arzneitaxe"), "Zusammensetzung der Rezeptur",choices = NULL, multiple = TRUE,
                          options = list(placeholder = "wähle Substanzen aus", maxItems = 1)), 
            numericInput(NS(id,"Menge_Substanz1"), "Menge der zu prüfende Substanz (g)", value = NULL, min = 0, max = 1, step = 0.01)
          )
        ),
        column(9,
          plotOutput(NS(id, "plot"), width = "100%", height = "120px"),#, click = NS(id,"arzneitaxe")), 
          tags$hr(),
          tableOutput(NS(id, "table"))
        )
      )
    )
  )
}


dosierungServer <- function(id, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      dosierung_lokal <- read.delim2("./data/NRF/Dosierung der Wirkstoffe zur Lokalanwendung.txt", header=FALSE)
      
      updateSelectizeInput(session, inputId = "arzneitaxe", choices = dosierung_lokal$V1, selected = Bestandteile, server = TRUE)
    
      calc_perce <- reactive({
        calc_perce <- (input$Menge_Substanz1/input$Mengeinsgesamt) * 100
        calc_perce
      })
    
      dataSet <- reactive({
        req(input$arzneitaxe)
        dataSet <- dosierung_lokal[which(dosierung_lokal$V1 == input$arzneitaxe),]
        dataSet
      })
    
      output$plot <- renderPlot({
        req(calc_perce())
        req(dataSet())
        
        ggplot(dataSet(), aes(x=V1))+
        geom_linerange(aes(ymin=V3,ymax=V4),linetype=1,color="blue")+
        geom_point(aes(y=V3),shape=15, size=3,color="blue")+
        geom_point(aes(y=V4),shape=15, size=3,color="blue")+
        geom_point(aes(y=calc_perce()),shape=8, size=5,color="red", na.rm = TRUE)+
        ylab("therapeutische Konzentration [%]")+
        ggtitle("therapeutische Konzentration [%]")+
        theme_bw()+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(
          plot.title = element_text(size=16, face="bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
        )+
        coord_flip()
      })
    
    not_within_con <- reactive({
      calc_perce()<dosierung_lokal[which(dosierung_lokal$V1 == input$arzneitaxe),]$V3 | calc_perce()>dosierung_lokal[which(dosierung_lokal$V1 == input$arzneitaxe),]$V4
    })
    
    output$table <- renderTable({
      req(input$arzneitaxe)
      df <- dosierung_lokal[which(dosierung_lokal$V1 == input$arzneitaxe),c(3,4)]
      df$V5 <- calc_perce()
      colnames(df) <- c("untere Grenze", "obere Grenze", "Konzentration Rezeptur")
      df
    })
    
    list(
      calc_perce = reactive(calc_perce()), 
      not_within_con = reactive(not_within_con())
    )
    
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
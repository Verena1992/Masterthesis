#2022_09

#input: rezeptpflichtdatensatz, Bestandteile der Rezeptur
#output: names and rezeptpflichtstatus of substances listed in Rezeptpflichtverordnung 
#        that are similar as Bestandteile der Rezeptur




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
rezeptpflichtUI <- function(id) {
  tagList(
    selectizeInput(NS(id,"WS"), "Wirkstoff",choices = NULL, multiple=TRUE,options = list(placeholder = "wähle Substanzen aus") ),
    textOutput(NS(id, "Rstatus")), 
    tableOutput(NS(id, "match"))
  )
}


#Server-------------------------------------------------
rezeptpflichtServer <- function(id, rezeptpflichtDS, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      updateSelectizeInput(session, "WS", choices = rezeptpflichtDS$Wirkstoff, server = TRUE
      )
      
      output$Rstatus <- renderPrint({
        selected_ws <- rezeptpflichtDS[which(rezeptpflichtDS$Wirkstoff == input$WS),]
        Rstatus <- selected_ws$Rstatus
        Rstatus
      })
      
      
      
      sub_rezeptpflicht <- reactive({
        req(Bestandteile)
        matches <- sapply(rezeptpflichtDS$Wirkstoff, function(x) grepl(x,Bestandteile, ignore.case = T))
        
        t <- which(matches, arr.ind = TRUE)

        
        if (length(Bestandteile) == 1 & !is_empty(t)){
          sub <- rezeptpflichtDS[t,]
        } else if (!is_empty(t)){
        
        sub <- rezeptpflichtDS[t[,2],]}
        else {
          sub <- NULL
        }
        
        sub
      })
      
      
      output$match <- renderTable({
        sub_rezeptpflicht()
      })
      
      list(
        element_rpf = reactive(sub_rezeptpflicht())
      )
      
    }
  )
}





# 
# 
# #testApp---------------------------------------------------------------
# rezeptpflichtApp <- function(){
#   ui <- fluidPage(
#     rezeptpflichtUI("rezeptpflicht")
# 
#   )
# 
#   server <- function(input, output, session){
#     Rezepturzusammensetzung <- c("ABACAVIR", "ABARELIX")
#   #  Rezepturzusammensetzung <- c("Atropinsulfat")
#     #Rezepturzusammensetzung <- c("ATROPINSULFAT")
#     rezeptpflichtServer("rezeptpflicht", rezeptpflichtDS, Rezepturzusammensetzung)
# 
# 
# 
#   }
# 
#   shinyApp(ui, server)}
# 
# rezeptpflichtApp()


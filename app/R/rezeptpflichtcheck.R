#2022_09

#input: rezeptpflichtdatensatz, Bestandteile der Rezeptur
#output: names and rezeptpflichtstatus of substances listed in Rezeptpflichtverordnung 
#        that are similar as Bestandteile der Rezeptur

#Beschreibung: Gibt Hinweise ob es für einen Bestandteil der Rezeptur eine ähnlichheißende Substanz Substanz in der
#Rezeptpflichtverordnung gelistet ist. Gibt auch die Möglichkeit Substanzen in der Rezeptpflichtverordnung zu suchen.


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
    tableOutput(NS(id, "match")),
    
    tags$hr(),
    
    selectizeInput(NS(id, "WS"), "Wirkstoff",choices = NULL, multiple = F, options = list(placeholder = "wähle Substanzen aus")),
    textOutput(NS(id, "Rstatus"))
  )
}


#Server-------------------------------------------------
rezeptpflichtServer <- function(id, rezeptpflichtDS, Bestandteile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
      updateSelectizeInput(session, "WS", choices = rezeptpflichtDS$Wirkstoff, server = TRUE,
                           selected=character(0)
      )
      
      output$Rstatus <- renderPrint({
        selected_ws <- rezeptpflichtDS[which(rezeptpflichtDS$Wirkstoff == input$WS),]
        Rstatus <- selected_ws$Rstatus
        Rstatus
      })
      
      
      
      sub_rezeptpflicht <- reactive({
        req(Bestandteile)
        #check if a substance listed in rezeptpflichtverordnung can be found in one of the Bestandteile of the Rezeptur
        #in der rezeptpflichtverordnung sind keine Salze gelisted, in Bestandteile von Rezepturen aber schon.
        #z.B. Rezeptplichtverordnung -- Atropin, Bestandteile -- Atropinsulfat
        #deshalb wird getestet ob eine Substanz der Rezeptpflichtverordnung in einem der Bestandteile vorkommt. 
        #ignore case -- in Rezeptpflichtverordnung oft alles Großbuchstaben
        matches <- sapply(rezeptpflichtDS$Wirkstoff, function(x) grepl(x,Bestandteile, ignore.case = T))
        
        #arr.ind - weil matches eine matrix
        t <- which(matches, arr.ind = TRUE)

        #wenn nur eine Substanz dann ist t keine Matrix sondern Vektor
        #wenn keine Substanz gefunden, entsteht sub ohne Werte aber bei is_empty == False
        #damit keine roter Button obwohl keine werte in sub -- return null wenn t is empty
        if (length(Bestandteile) == 1 & !is_empty(t)) {
          sub <- rezeptpflichtDS[t,]
        } else if (!is_empty(t)) {
          #in t ist jede Spalte eine Substanz aus der Rezeptpflichtverordnung und jede Zeile ein Bestandteil
          #nimm alle Spalten mit einem TRUE Wert
          sub <- rezeptpflichtDS[t[,2],]
        } else {
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
      
  })
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


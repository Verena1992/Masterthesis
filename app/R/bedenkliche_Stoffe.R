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

#---------------------------------------------------------------------------------------------------
bedenklichStUI <- function(id) {
  tagList(
    DT::dataTableOutput(NS(id,"mytable"))
  )
}


bedenklichStServer <- function(id, Rezepturzusammensetzung, bedenkliche_St) {
  moduleServer(
    id,
    function(input, output, session) {
      
      #https://github.com/rstudio/DT/issues/902
      output$mytable <- DT::renderDataTable({
        if(is_empty(bedenkliche_Substanz())){
          bedenkliche_St
        } else {
        bedenkliche_St  %>%
          DT::datatable(
        options = list(search = list(search = bedenkliche_Substanz()), searchHighlight = TRUE)
        )}
      })
      
      bedenkliche_Substanz <- reactive({
        bedenkliche_Substanz <- intersect(Rezepturzusammensetzung(), bedenkliche_St$Stoffe)
        bedenkliche_Substanz
      })
      
      list(
        bedenkliche_Substanz = reactive(bedenkliche_Substanz()) 
      )
      

    }
  )
}

# 
# #Test module:
# 
# bedenklichStApp <- function() {
# ui <- fluidPage(
#   bedenklichStUI("arzneimittelkommission"),
#   textOutput("bs")
# )
# 
# server <- function(input, output, session) {
# 
#   Rezepturzusammensetzung <- c("Amygdalin", "irge")
#  # Rezepturzusammensetzung <- c()
#   bs <- bedenklichStServer("arzneimittelkommission", Rezepturzusammensetzung)
#   output$bs <- renderText({
#          bs$bedenkliche_Substanz()
#        })
# }
# 
# shinyApp(ui, server)}
# bedenklichStApp()
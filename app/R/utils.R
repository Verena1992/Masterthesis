load_libraries <- function() {
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
}


zip2dataSet <- function(datapath, filenr, header=T, sep = "\t") {
  #takes a zip folder as input and reads in one file which defined by filenr
  file_list <- unzip(datapath, list = T)
  dataSet <- read.table(unz(datapath,file_list[filenr,1]), header=header, sep = sep)
  dataSet
}


adorigin2dataframe <- function(df, ori) {
  #new column origin is added 
  #ori = 1 ------ Juniormed
  #ori = 2 ------ intern
  origin <- rep(ori, nrow(df))
  df <- cbind(df, origin)
}


# Substanzauswahl <- function(id){
#   selectizeInput(
#     id, label = NULL, choices = NULL, multiple = TRUE,
#     options = list(create = TRUE, placeholder = "wähle Substanz aus")
#   )
# }


big_yellow_button <- function(id, label){
  actionBttn(
    inputId = id,
    label = label,
    color = "warning",
    style = "fill", 
    size = "lg", 
    block = TRUE
  )
}


subsettingRSammlung <- function(Substanzen, Rezeptursammlung) {
  for (i in Substanzen){
    Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == i),]$V1)
  }
  Rezeptursammlung
}



Substanzauswahl <- function(id){
  wellPanel(style = "height:80px",    
            fluidRow(
              column(4,
                     selectizeInput(id, label = NULL, choices = NULL, multiple = TRUE,
                                    options = list(create = TRUE, placeholder = "wähle Substanz aus"))),
              column(2,
                     actionButton(paste0(id,"ad"), label = "ad")),
              column(2,
                     numericInput(paste0(id, "num"), NULL , 1, min = 0, max = 1000)
              ),
              column(2,
                     switchInput(
                       inputId = paste0(id, "Einheit"),
                       label = "Einheit",
                       onLabel = "ml",
                       offLabel = "g"
                     ))
            ))
}
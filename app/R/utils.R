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
  library(shinyBS)
  library(pdftools)
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

update_Herstellungshinweise <- function(new_Herstellungshinweis, interne_Herstellungshinweise) {
  titel <- c("Titel", "Herstellungshinweise", "Quelle", "Dosierung", 
             "Haltbarkeit", "Lagerung", "Anwendung")
  colnames(interne_Herstellungshinweise) <- titel
  
  updated_Herstellungshinweise <- rbind(new_Herstellungshinweis,interne_Herstellungshinweise[-1,])
  updated_Herstellungshinweise
}

update_Rezeptur_Zusam <- function(new_Rezeptur_Zusam,interne_Rezeptursammlung) {
  titel <- rep(new_Herstellungshinweis()[1], length(new_Rezeptur_Zusam))
  new_Rezeptur_Zusam_df <- cbind(titel, new_Rezeptur_Zusam)
  colnames(new_Rezeptur_Zusam_df) <- c("V1", "V2")
  updated_Rezeptur_Zusam <- rbind(interne_Rezeptursammlung, new_Rezeptur_Zusam_df)
  updated_Rezeptur_Zusam
}


parse_NRF_verdrängungsfaktoren <- function(uploaded_file){
  AnlageF <- unlist(pdf_text(uploaded_file))
  # AnlageF <- unlist(pdf_text("~/data/anlage-f_el2021-1_2800.pdf"))
  lin <- unlist(strsplit(AnlageF, "\n"))
  first <- grep("Acetylsalicylsäure", lin)
  last <- grep("Zinkoxid", lin)
  wirkstoff <- c()
  for (i in first:last){
    a <- lin[i]
    #delet all empty spaces in each line
    b <- gsub("  ", "", as.character(a))
    #add a tabulator between Wirkstoff and Verdrängungsfaktor.
    #change comma to point
    c <-gsub("0,", "\t0.", as.character(b))
    wirkstoff <- append(wirkstoff, c)
  }
  d <- (wirkstoff[grep("\t0.", wirkstoff)])
  
  Wirkstoff <- c()
  Verdrängungsfaktor <- c()
  
  for (i in 1:length(d)){
    x <- strsplit(d[i], "\t")
    #append text for Ammoniumbituminosulfonat (from a 2.line) 
    if (x[[1]][1] ==  "Ammoniumbituminosulfonat/") {
      
      Wirkstoff <- append(Wirkstoff, "Ammoniumbituminosulfonat/ Glycerol 85%-Mischung 1:1" )
      Verdrängungsfaktor <- append(Verdrängungsfaktor, "0.80")
      #skip Ammoniumbituminosulfonat/Wasser, Verdrängungsfaktor not for Hartfett
      
      
    } else if (x[[1]][1] != "Ammoniumbituminosulfonat/Wasser-Mischung 1:1["){
      Wirkstoff <- append(Wirkstoff, x[[1]][1])
      Verdrängungsfaktor <- append(Verdrängungsfaktor, x[[1]][2])
    } else {
      print("skip")
    }
  }
  dataSet <- data.frame(Wirkstoff, Verdrängungsfaktor)
  return(dataSet)
}


find_verdrängungsf <- function(WS_S, Substanz_hinzufügen, new_verdrängungsfak, dataSet) {
 # dataSet <- data_Verdrän()
  vf <- dataSet[which(dataSet$Wirkstoff == WS_S),]
  if (Substanz_hinzufügen){
    vf <- new_verdrängungsfak
  } else {
    vf <- vf$Verdrängungsfaktor}
  return(vf)
}
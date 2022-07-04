zip2dataSet <- function(datapath, filenr, header=T, sep = "\t") {
  #takes a zip folder as input and reads in one file which defined by filenr
  file_list <- unzip(datapath, list = T)
  dataSet <- read.table(unz(datapath,file_list[filenr,1]), header=header, sep = sep)
  dataSet
}


Substanzauswahl <- function(id){
  selectizeInput(
    id, label = NULL, choices = NULL, multiple = TRUE,
    options = list(create = TRUE, placeholder = "wähle Substanz aus")
  )
}

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

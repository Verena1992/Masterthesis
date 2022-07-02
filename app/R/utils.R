

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

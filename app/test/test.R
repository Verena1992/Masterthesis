zip2dataSet <- function(datapath, filenr, header=T, sep = "\t") {
  #takes a zip folder as input and reads in one file which defined by filenr
  
  file_list <- unzip(datapath, list = T)
  #file_list <- unzip(datapath, list = T, exdir = getwd())
  browser()
  dataSet <- read.table(unz(datapath,file_list[filenr,1]), header=header, sep = sep)
  dataSet
}

adorigin2dataframe <- function(df, ori) {
  #ori = 1 ------ Juniormed
  #ori = 2 ------ intern
  origin <- rep(ori, nrow(df))
  df <- cbind(df, origin)
}










createRezeptursammlungUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), "Upload Zip file", accept = ".zip")
  )
}


createRezeptursammlungServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    #readin interne Rezeptursammlung
    interne_Rezeptursammlung <- reactive({
      req(input$file)
      dataSet <- zip2dataSet(input$file$datapath, filenr = 1)
      
      dataSet <- adorigin2dataframe(dataSet,2)
      dataSet
    })
    
    #merge juniormed with interne if uploaded
    rezeptursammlung <- reactive({
      if (!is.null(input$file)) {
        rezeptursammlung_Jun <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
        dataSet <- rbind(interne_Rezeptursammlung(), rezeptursammlung_Jun)
      } else {
        rezeptursammlung_Jun <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
        rezeptursammlung_Jun <- adorigin2dataframe(rezeptursammlung_Jun,1)
      }
    })
    list(
      rezeptursammlung = reactive(rezeptursammlung()),
      datapath = reactive(input$file$datapath)
    )
    # return(reactive(input$file$datapath))
  })
}















library(shiny)

ui <- fluidPage(
  createRezeptursammlungUI("jun_and_int"),
  uiOutput("selectizeInput01")
)

server <- function(input, output, session) {
  rz <- createRezeptursammlungServer("jun_and_int")
  # rezeptursammlung <- rz$rezeptursammlung()   # browser()
  output$selectizeInput01 <- renderUI({
    
    selectizeInput("Substanz", "Zusammensetzung der Rezeptur",choices = rz$rezeptursammlung()$V2, multiple = TRUE,
                   options = list(placeholder = "wähle Substanzen aus"))
    
    
  })
  
}

shinyApp(ui, server)



#02/07/2022
#1. read in Juniormed Rezeptursammlung convert it to dataframe
#2. (optional) upload, unzip, read in of private Rezeptursammlung
#3. merge Juniormed with optional uploaded private Rezeptursammlung.




#functions----------------------------------------------

zip2dataSet <- function(datapath, filenr, header=T, sep = "\t") {
  #takes a zip folder as input and reads in one file which defined by filenr
  file_list <- unzip(datapath, list = T, exdir = getwd())
  dataSet <- read.table(file_list[filenr,1], header=header, sep = sep)
  dataSet
}

adorigin2dataframe <- function(df, ori) {
  #ori = 1 ------ Juniormed
  #ori = 2 ------ intern
  origin <- rep(ori, nrow(df))
  df <- cbind(df, origin)
}

#dataSet <- zip2dataSet("./interne_Rezeptursammlung.zip", filenr = 1)
#dataSet2 <- zip2dataSet("./interne_Rezeptursammlung.zip", filenr = 2, header=F, sep = ";")

#data <- adorigin2dataframe(dataSet,2)

#UI-----------------------------------------------------

createRezeptursammlungUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), "Upload Zip file", accept = ".zip")
  )
}


#Server-------------------------------------------------

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

  })
}





#Test module:

RezeptursammlungApp <- function() {
  ui <- fluidPage(
    createRezeptursammlungUI("jun_and_int"),
    uiOutput("selectizeInput01"),
    tableOutput("text")
  )

  server <- function(input, output, session) {
    #create ui to select Substanzen from sammlung
    rezeptursammlung <- createRezeptursammlungServer("jun_and_int")
    
    output$selectizeInput01 <- renderUI({

         selectizeInput("Substanz", "Zusammensetzung der Rezeptur",choices = rezeptursammlung()$V2, multiple = TRUE,
                        options = list(placeholder = "wähle Substanzen aus"))


    })
   output$text <- renderTable(rezeptursammlung())
  }

  shinyApp(ui, server)
}

 RezeptursammlungApp()

# #02/07/2022 
# #1. read in Rezeptursammlung and selected Substanzen 
# #2. subsetting Rezeptursammlung with selected Substanzen
# #3. output Buttons

Rezeptursammlung <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
Substanzen <- c("Atropinsulfat", "Natriumchlorid")
juniormed_pagenr <- readRDS("~/data/Juniormed/juniormed_pagenr.rds")

#functions----------------------------------------------
subsettingRSammlung <- function(Substanzen, Rezeptursammlung) {
  for (i in Substanzen){
    Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == i),]$V1)
  }
  Rezeptursammlung
}

adorigin2dataframe <- function(df, ori) {
  #ori = 1 ------ Juniormed
  #ori = 2 ------ intern
  origin <- rep(ori, nrow(df))
  df <- cbind(df, origin)
}


zip2dataSet <- function(datapath, filenr, header=T, sep = "\t") {
  #takes a zip folder as input and reads in one file which defined by filenr
  file_list <- unzip(datapath, list = T)
  dataSet <- read.table(unz(datapath,file_list[filenr,1]), header=header, sep = sep)
  dataSet
}

Rezeptursammlung <- adorigin2dataframe(Rezeptursammlung, 1)

#UI-----------------------------------------------------
foundRezepturenButtonUI <- function(id) {
  tagList(
    uiOutput(NS(id,"Rezepturen")),
    uiOutput(NS(id,"Herstellungshinweis")),
    tableOutput(NS(id,"Herstellungstext_int"))
  )
}


#Server-------------------------------------------------
foundRezepturenButtonServer <- function(id, Substanzen, Rezeptursammlung,datapathHH) {
  moduleServer(id, function(input, output, session) {
    
    Rezeptur <- reactiveVal()
    
    
    subRezepturSammlung <- subsettingRSammlung(Substanzen, Rezeptursammlung)
    Rezepturen <- unique(subRezepturSammlung$V1)
    
    Rezeptur(Rezepturen)
    
    output$Rezepturen <- renderUI({
      ns <- session$ns
      lapply(1:length(Rezepturen), function(i){
        numlines <- which(Rezeptursammlung$V1 == Rezepturen[i])
        Bestandteile <- Rezeptursammlung$V2[numlines]
        
        
  
          actionButton(ns(paste0("Rezeptur",i)),HTML(paste0("<h3>",Rezepturen[i]),"</h3>", "<br/>", Bestandteile))
        
      })
    }) 

   
   #  ns <- session$ns
        lapply(1:length(Rezepturen), function(i){
            observeEvent(input[[paste0("Rezeptur", i)]], {
              numlines <- which(Rezeptursammlung$V1 == Rezepturen[i])
              print(Rezeptursammlung$origin[numlines])
              if ( unique(Rezeptursammlung$origin[numlines]) == 1)  {
                
              
                #JUN <- sub(".*JUN", "JUN", Rezepturen)
                JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
                print(JUN)
                src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
                print(src)
                #print(Rezeptur()[i])
                output$Herstellungshinweis <- renderUI({
                  
                  tags$iframe(src=src, height=500, width=800 )
                })
              } else {
                
                selected_int_Rezeptur <- reactiveValues(num = FALSE)
                
                interne_Herstellungshinweise <- reactive({
                 # req(input$file)
                  
                  dataSet <- zip2dataSet(datapathHH, filenr = 2, header=F, sep = ";")
                  dataSet
                })
                
                
                JUN_int <-  Rezeptur()[i]
                print(JUN_int)
                
                
                interne_Herstellungshinweise <- interne_Herstellungshinweise()
                number <- which(interne_Herstellungshinweise$V1 == JUN_int)
                print(number)
                selected_int_Rezeptur$num <- number
        
                table_int_sel_rezeptursammlung <- reactive({
                      number <- selected_int_Rezeptur$num
                      a <- as.data.frame(t(interne_Herstellungshinweise[c(1,number),]))
                      req(selected_int_Rezeptur$num)
                      colnames(a) <- c(unlist(interne_Herstellungshinweise[1]))
                      b <- a[-1,]
                      b
                    })

                  output$Herstellungstext_int <- renderTable(
                    table_int_sel_rezeptursammlung()
                  )
              }
              })
            })
})
}

#Test module:
# 
# foundRezepturenButtonApp <- function() {
#   ui <- fluidPage(
#     foundRezepturenButtonUI("button"),
#     #textOutput("text")
# 
#   )
# 
#   server <- function(input, output, session) {
#     Rezeptursammlung1 <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
#     Rezeptursammlung2 <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
#     Rezeptursammlung1 <- adorigin2dataframe(Rezeptursammlung1, 1)
#     Rezeptursammlung2 <- adorigin2dataframe(Rezeptursammlung2, 2)
#     Rezeptursammlung <- rbind(Rezeptursammlung2, Rezeptursammlung1)
#     Substanzen <- c("Atropinsulfat", "Natriumchlorid")
#   #  Substanzen <- c("Hartfett")
#     datapathHH <- c("interne_Rezeptursammlung.zip")
#     foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung, datapathHH)
#     #Rezepturen <- foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung)
#     #output$text <- renderText(foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung))
#   }
# 
#   shinyApp(ui, server)
# }
#
#foundRezepturenButtonApp()










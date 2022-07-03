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

Rezeptursammlung <- adorigin2dataframe(Rezeptursammlung, 1)

#UI-----------------------------------------------------
foundRezepturenButtonUI <- function(id) {
  tagList(
    uiOutput(NS(id,"Rezepturen")),
    uiOutput(NS(id,"Herstellungshinweis"))
  )
}


#Server-------------------------------------------------
foundRezepturenButtonServer <- function(id, Substanzen, Rezeptursammlung) {
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

   
      ns <- session$ns
        lapply(1:10, function(i){
            observeEvent(input[[paste0("Rezeptur", i)]], {
              #JUN <- sub(".*JUN", "JUN", Rezepturen)
              JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
              print(JUN)
              src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
              print(src)
              #print(Rezeptur()[i])
              output$Herstellungshinweis <- renderUI({
                
                tags$iframe(src=src, height=500, width=800 )
              })
              }, ignoreInit = TRUE)
            })
          
    
})
}

#Test module:

foundRezepturenButtonApp <- function() {
  ui <- fluidPage(
    foundRezepturenButtonUI("button"),
    #textOutput("text")
    
  )

  server <- function(input, output, session) {
    Rezeptursammlung <- read.csv("./Rezeptursammlung.txt", header=FALSE, sep=";")
    Substanzen <- c("Atropinsulfat", "Natriumchlorid")
    foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung)
    #Rezepturen <- foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung)
    #output$text <- renderText(foundRezepturenButtonServer("button",Substanzen, Rezeptursammlung))
  }

  shinyApp(ui, server)
}
#
foundRezepturenButtonApp()

# output$Herstellungshinweis <- renderUI({
#     lapply(1:length(Rezepturen), function(i){
#         observeEvent(ns(input[[paste0("Rezeptur", i)]]), {
#           #JUN <- sub(".*JUN", "JUN", Rezepturen)
#           JUN <- sub(".*JUN", "JUN", Rezeptur()[i])
#           print(JUN)
#           src <- juniormed_pagenr[which(juniormed_pagenr$JUN == JUN),]$unlist.url_JUN.
#           print(src)
#           #print(Rezeptur()[i])
#           tagList(
#             tags$iframe(src=src, height=500, width=800 )
#           )
#           })
#         })
#       })








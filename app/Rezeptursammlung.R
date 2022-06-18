
Rezeptursammlung <- read.csv("~/Rezeptursammlung.txt", header=FALSE, sep=";")

#install.packages("shinyWidgets")
library(shinyWidgets)

which(Rezeptursammlung$V2 == "Atropinsulfat")
a <- Rezeptursammlung[which(Rezeptursammlung$V2 == "Atropinsulfat"),]$V1


Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% a)

c <- Rezeptursammlung[which(Rezeptursammlung$V2 == "Natriumedetat"),]$V1

Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% c)
b

lapply (1:2, (function(i){
  
 #  Rezeptursammlung[which(Rezeptursammlung$V2 == paste0("Atropinsulfat", i)),]$V1
  
  
  Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% Rezeptursammlung[which(Rezeptursammlung$V2 == "Atropinsulfat"),]$V1)
  
}))
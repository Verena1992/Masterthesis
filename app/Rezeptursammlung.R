
Rezeptursammlung <- read.csv("~/Rezeptursammlung.txt", header=FALSE, sep=";")

install.packages("shinyWidgets")
library(shinyWidgets)

which(Rezeptursammlung$V2 == "Atropinsulfat")
a <- Rezeptursammlung[which(Rezeptursammlung$V2 == "Atropinsulfat"),]$V1


Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% a)

c <- Rezeptursammlung[which(Rezeptursammlung$V2 == "Natriumedetat"),]$V1

Rezeptursammlung <- subset(Rezeptursammlung, V1 %in% c)
b


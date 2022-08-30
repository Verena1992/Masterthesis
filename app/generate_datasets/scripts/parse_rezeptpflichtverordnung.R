install.packages("unrtf")
library(unrtf)

#download rtf-file from RIS and convert it to plain text
text <- unrtf("https://www.ris.bka.gv.at/GeltendeFassung/Bundesnormen/10010358/Rezeptpflichtverordnung%2c%20Fassung%20vom%2030.03.2022.rtf", format = "text")

#split text into separated lines
lin <- unlist(strsplit(text, "\n"))

#separate words
word <- strsplit(lin, "\t")



#define beginning and end of list
#nur Teil 2 human
last_wirkstoff <- which(lin == "\tZUCLOPENTHIXOL\tRP")
first_wirkstoff <- which(lin == "\tABACAVIR\tNR")


#extraxt Wirstoffe and there status
#word[[first_wirkstoff]] = ""         "ABACAVIR" "NR"
#usually wirkstoff is the second element and status the 3.
#in some cases(when the wirkstoff is a subclass of an other wirkstoff)-- length = 4
#when changing fist letter of the wirkstoffname there are empty lines
#additionaly some lines have special elements these are stored in vector "wrong"

Wirkstoff <- c()
Rstatus <- c()
wrong <- c()
position <- c()


for (i in first_wirkstoff:last_wirkstoff){
  if (length((word[[i]])) == 3){
    vector <- word[[i]]
    Wirkstoff <- append(Wirkstoff, vector[2])
    Rstatus <- append(Rstatus, vector[3])
  } else if (length((word[[i]])) == 4){
    vector <- word[[i]]
    Wirkstoff <- append(Wirkstoff, vector[3])
    Rstatus <- append(Rstatus, vector[4])    
  } else if (length((word[[i]])) == 2 & identical(word[[i]][1], "") & identical(word[[i]][2], "")){
    print("skip empty lines")
  } else {
    position <- append(position, i)
    wrong <- append(wrong, word[[i]])
  }
}

wrong
position


#change names of Wirkstoffe with subgroups and there subgroups
Wirkstoff[which(Wirkstoff == "Ameisensäure")] <- "Ameisensäure - Ausnahme (Formiate)"
Wirkstoff[which(Wirkstoff == "Antimon und seine Verbindungen")] <- "Antimon und seine Verbindungen - Ausnahme  (Salze der Antimonylweinsäure)"
Wirkstoff[which(Wirkstoff == "Bariumsalze")] <- "Bariumsalze - Ausnahmen (Bariumsulfat,  Bariumsalze als Röntgenkontrastmittel)"
Wirkstoff[which(Wirkstoff == "als Röntgenkontrastmittel")] <- "Bariumsalze, als Röntgenkontrastmittel"
Wirkstoff[which(Wirkstoff == "Chinin")] <- "Chinin - Ausnahmen (Chinin-Eisen-citrat, Chinin-tannat)"
Wirkstoff[which(Wirkstoff == "Eisen (III)-ammonium citrat")] <- "Eisen (III)-ammonium citrat - Ausnahme (als Diagnostikum)"
Wirkstoff[which(Wirkstoff == "Eisen (III)-ammonium citrat")+1] <- "Eisen (III)-ammonium citrat -- als Diagnostikum"
Wirkstoff[which(Wirkstoff == "Eisen (II) – Salze und Chelate")] <-  "Eisen (II) – Salze und Chelate - Ausnahme (Chinin-Eisen-Citrat)"
Wirkstoff[which(Wirkstoff == "Fluoride und Fluorkomplexe")] <- "Fluoride und Fluorkomplexe - Ausnahme (DECTAFLUR, Natriumfluorid, OLAFLUR)"
Wirkstoff[which(Wirkstoff == "Histamin")] <- "Histamin - Ausnahme (als Diagnostikum)"
Wirkstoff[which(Wirkstoff == "Histamin")+1] <- "Histamin als Diagnostikum"
Wirkstoff[which(Wirkstoff == "ISOTRETINOIN")] <- "ISOTRETINOIN - Ausnahme (für äußerliche Anwendung)"
Wirkstoff[which(Wirkstoff == "ISOTRETINOIN")+1] <- "ISOTRETINOIN, für äußerliche Anwendung"
Wirkstoff[which(Wirkstoff == "Manganverbindungen")] <- "Manganverbindungen - Ausnahme (Kaliumpermanganat)"
Wirkstoff[which(Wirkstoff == "Quecksilber und seine Verbindungen")] <- "Quecksilber und seine Verbindungen - Ausnahme (Quecksilberamidochlorid, Rotes Quecksilbersulfid)"
Wirkstoff[which(Wirkstoff == "Salicylsäure")] <- "Salicylsäure - Ausnahme (Salze, Ester)"
Wirkstoff[which(Wirkstoff == "Salicylsäure")+1] <- "Salze und Ester der Salicylsäure"
Wirkstoff[which(Wirkstoff == "Silber und seine Verbindungen")] <- "Silber und seine Verbindungen - Ausnahme (Silberchlorid, Stylus Argentici nitrici cum Kalio nitrico)" 
Wirkstoff[which(Wirkstoff == "Strontiumverbindungen")] <- "Strontiumverbindungen - Ausnahme (Strontiumchlorid)"
Wirkstoff[which(Wirkstoff == "Zinksulfat")] <- "Zinksulfat - Ausnahme (Collyrium Zinci)"


#append wirkstoffe with special elements
Wirkstoff <- append(Wirkstoff, c("alpha-Acetyldigoxin", "beta-Acetyldigoxin", "BAZEDOCIFEN", "CEFTAROLINFOSAMIL", "GRAZOPREVIR", 
                                 "MIKROBIELLE COLLAGENASE aus CLOSTRIDIUM HISTOLYTICUM", "MIRABEGRON", "PERTUZUMAB", "REGORAFENIB", "SIPULEUCEL-T"))
Rstatus <- append(Rstatus, c("RP","RP", "NR", "NR", "NR", "NR", "RP", "NR", "NR", "NR"))



rezeptpflicht <- data.frame(Wirkstoff, Rstatus)
saveRDS(rezeptpflicht, file ="data/Rezeptpflicht/rezeptpflicht.rds")



#test if number of status in text is the same as rows in dataframe
#1773 rows, some 1772, one missing????
a <- sum(unlist(word[first_wirkstoff:last_wirkstoff]) == "RP")
b <- sum(unlist(word[first_wirkstoff:last_wirkstoff]) == "NR")
c <- sum(unlist(word[first_wirkstoff:last_wirkstoff]) == "RPF")

Teil2_human <- word[first_wirkstoff:last_wirkstoff]

Teil2_human <- unlist(word[first_wirkstoff:last_wirkstoff])
d <- sum(grepl("RP,",Teil2_human, fixed = TRUE))
e <- sum(grepl("NR,",Teil2_human, fixed = TRUE))
f <- sum(grepl("RPF,",Teil2_human, fixed = TRUE))

a + b + c + d + e + f




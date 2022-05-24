
library(unrtf)




#download rtf-file from RIS and convert it to plain text
text <- unrtf("https://www.ris.bka.gv.at/GeltendeFassung/Bundesnormen/10010306/%c3%96sterreichische%20Arzneitaxe%201962%2c%20Fassung%20vom%2016.05.2022.rtf", format = "text")
text <- unrtf("~/data/Arzneitaxe/Österreichische Arzneitaxe 1962, Fassung vom 16.05.2022.rtf", format = "text")

head(text)

#split text into separated lines
lin <- unlist(strsplit(text, "\n"))

#separate words
word <- strsplit(lin, "\t")

last_wirkstoff <- which(lin == "\tZypressenöl\t1\t58")
first_wirkstoff <- which(lin == "\tAbführender Tee offizinal\t10\t52")

Wirkstoff <- c()

for (i in first_wirkstoff:last_wirkstoff){
    vector <- word[[i]]
    Wirkstoff <- append(Wirkstoff, vector[2])

}
head(Wirkstoff)

#removing *) from string
Wirkstoff <- gsub(" \\*)", "", as.character(Wirkstoff))

saveRDS(Wirkstoff, file ="data/Arzneitaxe/Arzneitaxe.rds")

#add erstattungssstatus to Arzneitaxe Wirkstoffe

eko <- read.delim(file = "~/data/EKO/Stoffe_gelbeListe_EKO.txt", sep = "\t", header = T)
eko$Stoffe[1]

found_wirkstoff <- c()
missing_wirkstoff <- c()

#for (i in Wirkstoff){
#  for (j in 1:length(eko$Stoffe)){
#    if (eko$Stoffe[j]==i){
#      found_wirkstoff <- append(found_wirkstoff, i)
#    } else if (any(Wirkstoff==eko$Stoffe[j])){
#      missing_wirkstoff <- append(missing_wirkstoff, j)
#    }
#  }
#} 

for (j in 1:length(eko$Stoffe)){
  if (!any(Wirkstoff==eko$Stoffe[j])){
    missing_wirkstoff <- append(missing_wirkstoff, eko$Stoffe[j])
  }
}

for (j in 1:length(eko$Stoffe)){
  if (!any(Wirkstoff==eko$Stoffe[j])){
    missing_wirkstoff <- append(missing_wirkstoff, eko$Stoffe[j])
  }
}

for (j in 1:length(eko$Stoffe)){
  print(eko$Stoffe[j])
  print(Wirkstoff[grep(eko$Stoffe[j], Wirkstoff)])
  #print(eko$Stoffe[j])
 # indi <- grep(eko$Stoffe[j], Wirkstoff)
#    print(i)
}

eko$Stoffe[1]
Wirkstoff[grep(eko$Stoffe[1], Wirkstoff)]






for (j in 1:length(eko$Stoffe)){
    print(j)
}




any(rezeptpflicht=="ABACAVIR")

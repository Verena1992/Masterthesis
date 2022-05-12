rezeptpflicht <- readRDS("~/data/Rezeptpflicht/rezeptpflicht.rds")
library(unrtf)

#download rtf-file from RIS and convert it to plain text
text <- unrtf("https://www.ris.bka.gv.at/GeltendeFassung/Bundesnormen/10010306/%c3%96sterreichische%20Arzneitaxe%201962%2c%20Fassung%20vom%2011.05.2022.rtf", format = "text")
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

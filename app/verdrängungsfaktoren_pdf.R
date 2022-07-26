library(pdftools)
library(vroom)

AnlageF <- unlist(pdf_text("~/data/anlage-f_el2021-1_2800.pdf"))

lin <- unlist(strsplit(AnlageF, "\n"))

first <- grep("Acetylsalicylsäure", lin)
last <- grep("Zinkoxid", lin)

wirkstoff <- c()
for (i in first:last){
  a <- lin[i]
  #delet all empty spaces in each line
  b <- gsub("  ", "", as.character(a))
  #add a tabulator between Wirkstoff and Verdrängungsfaktor.
  #change comma to point
  c <-gsub("0,", "\t0.", as.character(b))
  wirkstoff <- append(wirkstoff, c)
}

d <- (wirkstoff[grep("\t0.", wirkstoff)])
                
Wirkstoff_pdf <- c()
Verdrängungsfaktor_pdf <- c()
                
 for (i in 1:length(d)){
     x <- strsplit(d[i], "\t")
     #append text for Ammoniumbituminosulfonat (from a 2.line) 
     if (x[[1]][1] ==  "Ammoniumbituminosulfonat/") {
            
     Wirkstoff_pdf <- append(Wirkstoff_pdf, "Ammoniumbituminosulfonat/ Glycerol 85%-Mischung 1:1" )
     Verdrängungsfaktor_pdf <- append(Verdrängungsfaktor_pdf, "0.80")
     #skip Ammoniumbituminosulfonat/Wasser, Verdrängungsfaktor not for Hartfett
                
     
       } else if (x[[1]][1] != "Ammoniumbituminosulfonat/Wasser-Mischung 1:1["){
          Wirkstoff_pdf <- append(Wirkstoff_pdf, x[[1]][1])
         Verdrängungsfaktor_pdf <- append(Verdrängungsfaktor_pdf, x[[1]][2])
                  } else {
                    print("skip")
                  }
                }
                
                
dataSet <- data.frame(Wirkstoff_pdf, Verdrängungsfaktor_pdf)

                
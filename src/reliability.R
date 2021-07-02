library(mokken)
library(DescTools)

#remove.packages(coefficientalpha)

data <- read.csv('data/master-data.csv', header = FALSE)

## Uji validitas variabel 1

MIN_ROW<-4
MAX_ROW<-118


checkReliability <- function(MIN_COL, MAX_COL, title) {
  
  baseData <- c()
  
  # should be reversed
  for (row in MIN_ROW:MAX_ROW) {
    for(col in MIN_COL:MAX_COL) {
      baseData <- append(baseData, as.integer(data[row, col]))
    }
  }
  
  fakeColName <- c()
  
  for(i in MIN_COL:MAX_COL) {
    fakeColName <- append(fakeColName, paste("per", i, sep=""))
  }
  
  tab <- matrix(baseData, ncol=(MAX_COL - MIN_COL + 1), byrow=TRUE)
  colnames(tab) <- fakeColName
  rownames(tab) <- c((MIN_ROW - (MIN_ROW - 1)):(MAX_ROW - (MIN_ROW - 1)))
  tab <- as.table(tab)
  
  
  tabResults <- CronbachAlpha(tab)
  resource <- check.reliability(tab)
  
  print(paste("Hasil Uji Reliabilitas Terhadap Variabel", title, "Adalah :"))
  ## Reliability by moken 
  print(tabResults)
  ## Reliability by Desc Tool cronbatch alpha
  print(resource)
}


checkReliability(1, 16, "ALOKASI")

checkReliability(17, 22, "INTERAKSI")

checkReliability(23, 27, "KONSULTASI")

checkReliability(28, 30, "REGISTRASI")

checkReliability(31, 34, "KONGITIF")

checkReliability(35, 36, "KONATIF")

checkReliability(37, 39, "AFEKTIF")

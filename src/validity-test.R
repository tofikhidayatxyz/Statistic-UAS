data <- read.csv('data/master-data.csv', header = FALSE)

## Uji validitas variabel 1

MIN_ROW<-4
MAX_ROW<-118

validityTest <- function(MIN_COL, MAX_COL, NAME) {
  
  baseData <- list()
  sumMaxData <- c()
  
  for (idx in MIN_COL:MAX_COL) {
    limData = data[MIN_ROW:MAX_ROW, idx]
    baseData <- append(baseData, list(as.numeric(unlist(limData))))
  }
  
  
  # recount max data 
  loopIdx <- 0;
  for(dataSource in baseData) {
    loopIdx <- loopIdx + 1
    rowIxd = 0
    for(row in dataSource) {
      rowIxd <- rowIxd + 1
      currentvalue <- ifelse(is.null(sumMaxData[rowIxd]) || is.na(sumMaxData[rowIxd]), 0, sumMaxData[rowIxd])
      sumMaxData[rowIxd] <- currentvalue + row
    }
  } 

  rowIxd = 0
  for(dataSource in baseData) {
    rowIxd <- rowIxd + 1
    print(paste("UJI VALIDITAS", NAME, rowIxd))
    print(cor.test(dataSource, sumMaxData))
  }
}

# call file
source(file="src/validity/alokasi.R")
source(file="src/validity/interaksi.R")
source(file="src/validity/konsultasi.R")
source(file="src/validity/registrasi.R")
source(file="src/validity/kognitif.R")
source(file="src/validity/konatif.R")
source(file="src/validity/afektif.R")
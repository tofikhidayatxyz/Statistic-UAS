library(data.table)

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
    restData <- cor.test(dataSource, sumMaxData)
    #print(restData)
    print(paste("Name : ", NAME, "p-value : ", restData$p.value, "cor : ", restData$estimate))
  }
}


# call file

validityTest(1, 16, "ALOKASI")

validityTest(17, 22, "INTERAKSI")

validityTest(23, 27, "KONSULTASI")

validityTest(28, 30, "REGISTRASI")

validityTest(31, 34, "KONGITIF")

validityTest(35, 36, "KONATIF")

validityTest(37, 39, "AFEKTIF")


MIN_COL<-28
MAX_COL<-30

baseData <- list()
sumMaxData <- c()

for (idx in MIN_COL:MAX_COL) {
  print(idx)
  limData = data[MIN_ROW:MAX_ROW, idx]
  baseData <- append(baseData, list(as.numeric(unlist(limData))))
}


# recount max data 
loopIdx <- 0;
for(dataSource in baseData) {
  loopIdx <- loopIdx + 1
  print(dataSource)
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
  print(paste("UJI VALIDITAS REGISTRASI", rowIxd))
  print(cor.test(dataSource, sumMaxData))
}
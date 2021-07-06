library(lmtest)

data <- read.csv('data/master-data.csv', header = FALSE)


MIN_ROW<-4
MAX_ROW<-118

xVar <- list(
  data.frame(
    minCol=1,
    maxCol=16,
    title="ALOKASI"
  ),
  data.frame(
    minCol=17,
    maxCol=22,
    title="INTERAKSI"
  ),
  data.frame(
    minCol=23,
    maxCol=27,
    title="KONSULTASI"
  ),
  data.frame(
    minCol=28,
    maxCol=30,
    title="REGISTRASI"
  )
)


yVar <- list(
  data.frame(
    minCol=31,
    maxCol=34,
    title="KONGITIF"
  ),
  data.frame(
    minCol=35,
    maxCol=36,
    title="KONATIF"
  ),
  data.frame(
    minCol=37,
    maxCol=39,
    title="AFEKTIF"
  )
)






for(x in xVar) {
  
  xData <- c()

  for(row in MIN_ROW:MAX_ROW) {
   for(xCol in x$minCol:x$maxCol) {
     cRow <- row - (MIN_ROW - 1)
     currentXData <- ifelse(is.na(xData[cRow]) || is.null(xData[cRow]), 0, xData[cRow])
     xData[cRow] <- (currentXData + as.integer(data[row, xCol]));
   }
  }
  
  
  
  ## get y data
  for(y in yVar) {
    yData <- c()
    for(yRow in MIN_ROW:MAX_ROW) {
      for(yCol in y$minCol:y$maxCol) {
        yXRow <- yRow - (MIN_ROW - 1)
        currentYData <- ifelse(is.na(yData[yXRow]) || is.null(yData[yXRow]), 0, yData[yXRow])
        yData[yXRow] <- (currentYData + as.integer(data[yRow, yCol]));
      }
    }
    
    print(paste("HASIL T TEST VARIABEL ", x$title, "TERHADAP VARIABEL", y$title, "ADALAH : "))
    res <- t.test(xData, yData)
    print(res)
  }
    
#    print(paste("HASIL T TEST VARIABEL ", x$title, "TERHADAP VARIABEL", y$title, "ADALAH : "))
#    resettest(xData~yData)
 # }
}


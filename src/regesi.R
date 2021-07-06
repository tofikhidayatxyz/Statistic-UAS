library(lmtest)

data <- read.csv('data/master-data.csv', header = FALSE)


getData <- function(MIN_COL, MAX_COL) {
  xData <- c()
  
  for(row in MIN_ROW:MAX_ROW) {
    for(xCol in MIN_COL:MAX_COL) {
      cRow <- row - (MIN_ROW - 1)
      currentXData <- ifelse(is.na(xData[cRow]) || is.null(xData[cRow]), 0, xData[cRow])
      xData[cRow] <- (currentXData + as.integer(data[row, xCol]));
    }
  }
  
  return(xData)
}

xData <- data.frame(
  ALOKASI = getData(1, 16),
  INTERAKSI = getData(17, 22),
  KONSULTASI = getData(23, 27),
  REGISTRASI = getData(28, 30)
)

yData <- data.frame(
  KONGITIF= getData(31, 34),
  KONATIF= getData(35, 36),
  AFEKTIF = getData(37, 9)
)

# regesi linier 
print("REGESI LINIER MULTI VARIAT TERHADAP Y KONGITIF")
summary(lm(yData$KONGITIF ~ xData$ALOKASI + xData$INTERAKSI + xData$KONSULTASI + xData$REGISTRASI))

print("REGESI LINIER MULTI VARIAT TERHADAP Y KONATIF")
summary(lm(yData$KONATIF ~ xData$ALOKASI + xData$INTERAKSI + xData$KONSULTASI + xData$REGISTRASI))

print("REGESI LINIER MULTI VARIAT TERHADAP Y AFEKTIF")
summary(lm(yData$AFEKTIF ~ xData$ALOKASI + xData$INTERAKSI + xData$KONSULTASI + xData$REGISTRASI))



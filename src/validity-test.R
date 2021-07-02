data <- read.csv('data/master-data.csv', header = FALSE)

## Uji validitas variabel 1

MIN_ROW<-4
MAX_ROW<-118

source(file="src/validity/alokasi.R")
setwd(dir = "/Users/will/Desktop/specdata")
openfiles <- function(pollutant, id = 1:332) {
  len <- length(id)
  sumvalue <- numeric(0)
  howmany <- numeric(0)
  for (i in id) {
    if (i < 10) { filename<- paste("00", i, ".csv", sep = "")} 
    else if (10 <= i & i < 100) {filename <- paste("0", i, ".csv", sep = "")} 
    else{filename <- paste(i, ".csv", sep = "")}
    readfile <- read.csv(filename)[[pollutant]]
    sumvalue <- rbind(sumvalue,sum(readfile[!is.na(readfile)]))
    howmany<- rbind(howmany, length(readfile) - sum(is.na(readfile)))}
  sum(sumvalue)/sum(howmany)}
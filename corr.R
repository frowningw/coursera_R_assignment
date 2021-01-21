setwd(dir = "/Users/will/Desktop/specdata")
corr <- function(threshold = 0) {
  thres <- numeric(0)
  for (i in 1:332) {
    if (i < 10) { filename<- paste("00", i, ".csv", sep = "")} 
    else if (10 <= i & i < 100) {filename <- paste("0", i, ".csv", sep = "")} 
    else{filename <- paste(i, ".csv", sep = "")}
    sul <- read.csv(filename)$sulfate
    nit <- read.csv(filename)$nitrate
    good <- complete.cases(sul, nit)
    if (sum(good)>threshold) {thres<-cbind(thres, cor(sul[good],nit[good]))}}
  thres[1:6]}
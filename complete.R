setwd(dir = "/Users/will/Desktop/specdata")
complete <- function(id = 1:332) {
  len <- length(id)
  nobs <- numeric(0)
  for (i in id){
    if (i < 10) {filename <- paste("00", i, ".csv", sep = "")} 
    else if (10 <= i & i < 100) {filename <- paste("0", i, ".csv", sep = "")} 
    else{filename <- paste(i, ".csv", sep = "")}
    sul <- read.csv(filename)$sulfate
    nit <- read.csv(filename)$nitrate
    nobs<-rbind(nobs,sum(complete.cases(sul, nit)))}
  cbind(id, nobs)}
setwd(dir="/Users/will/Desktop/rprog")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

rankall <- function(disease, num=1){
  if (!(disease %in% c("heart attack", "heart failure", "pneumonia")))stop("Invalid Outcome")
  ind <- 0
  if (disease == "heart attack"){ind<-11}
  else if (disease == "heart failure"){ind<-17}
  else {ind <- 23} 
  out <- outcome[,c(2, 7,ind)]      #2: hosp.name; 7: state; ind=(11,17,23), disease death rate
  out <- out[out[,3]!="Not Available",    ] #getting rid of NA
  out[, 3] <- as.numeric(out[,3])  #turning death rate col into numeric
  out_factor <- factor(out[,2]) #for leveling 
  Tlst <-vector("list",0)
  for (state in levels(out_factor)){
    Tout <- out[out$State==state, ]
    if (num =="best"){
      Tout <- Tout[order(Tout[,3],Tout[,1])[1],  ]
      Tlst<-rbind(Tlst,c(Tout[,1:2]))} 
    else if(num == "worst"){
      Tout <- Tout[order(Tout[,3],Tout[,1])[nrow(Tout)],  ]
      Tlst<-rbind(Tlst,c(Tout[,1:2]))} 
    else if (num > nrow(Tout)) {
      Tlst<-rbind(Tlst,c("NA",state))
    } else {
      Tout <- Tout[order(Tout[,3],Tout[,1])[num],  ]
      Tlst<-rbind(Tlst,c(Tout[,1:2]))
    }
  }
  Tlst
}
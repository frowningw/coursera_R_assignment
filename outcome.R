setwd(dir="/Users/will/Desktop/rprog")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
best <- function(state, disease){
  if (!(state %in% outcome[,7])) stop("Invalid State")
  if (!(disease %in% c("heart attack", "heart failure", "pneumonia")))stop("Invalid Outcome")
  ind <- 0
  if (disease == "heart attack"){ind<-11}
  else if (disease == "heart failure"){ind<-17}
  else {ind <- 23} 
  out <- outcome[,c(2,7,ind)][outcome[,c(2,7,ind)]$State==state,   ]
  Tout <- out[,c(1,3)][out[,3]!="Not Available",    ]     #T means targeted
  Tout[, 2] <- as.numeric(Tout[,2])
  Tout2 <- Tout[Tout[,2]==min(Tout[,2]),    ] 
  Tout2}

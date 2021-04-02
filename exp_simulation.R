lambda = 0.2 #exp dist property
En = c()
Vn = c()
m = 1000 #simulation times
op = par(mfrow=c(1,1)) #plot
n = 40 #sample size

#simulation begins
for (i in 1:m){
  
  #sample from exp
  W = rexp(n, lambda)
  
  #standardize
  En[i] = (mean(W) - 1/lambda)/((1/lambda)/sqrt(n))
  Vn[i] = sd(W)^2
}
#plot
head(En) #dist mean = 1/lambda = 5
head(Vn) #dist var = (1/lambda)^2 = 25
hist(En, probability = TRUE, breaks = seq(-10,10,by=.25), main = "1,000 samples from means of rexp(40, .2)")
curve(dnorm(x, mean= 0, sd=1), add= TRUE, col = "red")



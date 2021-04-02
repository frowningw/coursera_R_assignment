data <- ToothGrowth
summary(data)
head(data)
str(data)
library(dplyr)

#exploratory plotting
with(data, boxplot(len~supp))
with(data, boxplot(len~dose))

datavc <- data[1:30,1]
dataoj <- data[31:60,1]

#moments by supp
datasupp <- data %>%
  group_by(supp) %>%
  summarize(len_mu = mean(len), len_sd = sd(len), len_var = var(len))
datasupp
#moments by dose
datadose <- data %>%
  group_by(dose) %>%
  summarize(len_mu = mean(len), len_sd = sd(len), len_var = var(len))
datadose

#by supp, can be treated as 2 independent samples, each 30
#VC-OJ
s <- sqrt((29*(as.numeric(datasupp[1,4]) + 
                 as.numeric(datasupp[2,4])))/58)
intvl <- as.numeric(datasupp[2,2] - datasupp[1,2]) + c(-1,1)*qt(0.975, 58)*s*sqrt(1/15)
t_conf <- t.test(datavc, dataoj)$conf

#since the interval contains 0, 
#we cannot rule out the possibility that the supp sources are indifferent.

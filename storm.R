#Data Processing
setwd("~/Desktop")
file <- read.csv("repdata_data_StormData.csv")
library(dplyr)
library(tidyr)

harm_fatalities <- file %>% group_by(EVTYPE) %>% summarise(totalfatalities = sum(FATALITIES))
harm_injuries <- file %>% group_by(EVTYPE) %>% summarise(totalinjuries = sum(INJURIES))
harm <- merge(harm_fatalities, harm_injuries, by = "EVTYPE") %>%
  mutate(totaldmg = totalfatalities+totalinjuries) %>% 
  arrange(desc(totaldmg))
max_harm_type <- as.character(harm[1,1])
with(harm[1:5,], 
     barplot(totaldmg, 
             main = "HUMAN LOSS OF EVTYPE (TOP5)", 
             xlab = "EVTYPE",
             ylab = "lives claimed",
             names.arg= EVTYPE))

propecon <- file %>%
  select("EVTYPE", "PROPDMG", "PROPDMGEXP") %>%
  filter(PROPDMG != 0)%>%
  group_by(EVTYPE, PROPDMGEXP) %>%
  summarize(DMG = sum(PROPDMG))
prop_k <- propecon %>%
  filter(PROPDMGEXP=="K")
prop_m <- propecon %>%
  filter(PROPDMGEXP=="M")
prop_b <- propecon %>%
  filter(PROPDMGEXP=="B")
propecon <- rbind(prop_k, prop_m, prop_b)

for (i in c(1:length(propecon$PROPDMGEXP))) {
  if (propecon[i,2] == "K") {propecon[i,2] <- as.character(1000)}
  else if (propecon[i,2] == "M") {propecon[i,2] <- as.character(1000000)}
  else if (propecon[i,2] == "B") {propecon[i,2] <- as.character(1000000000)}}
propecon$PROPDMGEXP <- as.numeric(propecon$PROPDMGEXP)
propecon <- propecon %>%
  mutate(totaldmg = PROPDMGEXP*DMG) %>%
  group_by(EVTYPE) %>%
  summarize(dmg = sum(totaldmg)) %>%
  arrange(desc(dmg))

cropecon <- file %>% 
  select("EVTYPE", "CROPDMG", "CROPDMGEXP") %>%
  filter(CROPDMG != 0) %>%
  group_by(EVTYPE, CROPDMGEXP) %>%
  summarize(DMG = sum(CROPDMG))

for (i in c(1:length(cropecon$CROPDMGEXP))) {
  if (cropecon[i,2] == "K") {cropecon[i,2] <- as.character(1000)}
  else if (cropecon[i,2] == "M") {cropecon[i,2] <- as.character(1000000)}
  else if (cropecon[i,2] == "B") {cropecon[i,2] <- as.character(1000000000)}
  else if (cropecon[i,2] == "")  {cropecon[i,2] <- as.character(0)}}

cropecon <-cropecon%>%
  filter(CROPDMGEXP != "0")
cropecon$CROPDMGEXP <- as.numeric(cropecon$CROPDMGEXP)
cropecon <- cropecon %>%
  mutate(totaldmg = CROPDMGEXP*DMG) %>%
  group_by(EVTYPE) %>%
  summarize(dmg = sum(totaldmg)) %>%
  arrange(desc(dmg))

econdmg <- rbind(propecon, cropecon)
econdmg <- econdmg %>%
  group_by(EVTYPE) %>%
  summarize(totaldmg = sum(dmg)) %>%
  arrange(desc(totaldmg))
max_dmg_type <- as.character(econdmg[1,1])
with(econdmg[1:5,], 
     barplot(totaldmg, 
             main = "ECON LOSS OF EVTYPE (TOP5)", 
             xlab = "EVTYPE", 
             ylab = "$",
             names.arg= EVTYPE))

  




CPSdata <- load("~/UW Fall 2021/SOC 504 A/DATA_CPS_2016_2021.Rda")
install.packages("tidyverse")
library(haven)
CPSdata$RACE <- as_factor(CPSdata$RACE) # as_factor is supplied by the haven package
mean(CPS_2016_2021$WKSTAT)
median(CPS_2016_2021$WKSTAT)
mode(CPS_2016_2021$WKSTAT)
sd(CPS_2016_2021$WKSTAT)
Wkstat <- as.data.frame(table(CPS_2016_2021$WKSTAT))
Racedata<- as.data.frame(table(CPS_2016_2021$RACE))
RaceWS <- table(CPS_2016_2021$RACE, CPS_2016_2021$WKSTAT)
RaceWS <- as.data.frame(table(CPS_2016_2021$RACE & CPS_2016_2021$WKSTAT))


Wkstat[12,]
table(CPS_2016_2021$RACE, CPS_2016_2021$WKSTAT)
median(CPS_2016_2021$RACE, CPS_2016_2021$WKSTAT)
order(CPS_2016_2021$RACE)
table(CPS_2016_2021$SEX)

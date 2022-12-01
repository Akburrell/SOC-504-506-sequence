load(DATA_CPS_2016_2021.Rda)
my_cps <- CPS_2016_2021[ ,c("AGE", "HHTENURE")]
head(my_cps)
summary(my_cps$AGE)
AGE_t <- t.test(my_cps$AGE)
AGE_t
AGE_t$conf.int

library(haven)
my_cps$HHTENURE <- as.integer(my_cps$HHTENURE)
table(my_cps$HHTENURE)
home_owner <- my_cps[,c("HHTENURE")]
my_cps$home_owner[my_cps$HHTENURE == 01] <- "1"
my_cps$home_owner[my_cps$HHTENURE == 00] <- "0"
my_cps$home_owner[my_cps$HHTENURE == 02] <- "0"
my_cps$home_owner[my_cps$HHTENURE == 03] <- "0"
my_cps$home_owner[my_cps$HHTENURE == 06] <- "0"
my_cps$home_owner[my_cps$HHTENURE == 07] <- "0"
table(my_cps$home_owner)

homeowner<- my_cps[my_cps$home_owner == "1", ]
other<- my_cps[my_cps$home_owner == "0", ]
nrow(homeowner)
nrow(other)
head(other)
mean(other$AGE)
mean(homeowner$AGE)
mean(homeowner$AGE) - mean(other$AGE)
t_homeowner_other <- t.test(homeowner$AGE, other$AGE)
t_homeowner_other

SWRdata <- CPS_2016_2021[ ,c("SEX", "WKSTAT","RACE")]
nothing <- c(13,42,50,60,99)

clean_SWRdata <- SWRdata[SWRdata$WKSTAT != 13 & SWRdata$WKSTAT != 42 
                         & SWRdata$WKSTAT != 50 & SWRdata$WKSTAT !=60 & SWRdata$WKSTAT != 99
                         & SWRdata$RACE != 300 & SWRdata$RACE != 650  & SWRdata$RACE != 651 
                         & SWRdata$RACE != 652 & SWRdata$RACE != 700 &  SWRdata$RACE != 801
                         & SWRdata$RACE != 802 & SWRdata$RACE != 803 & SWRdata$RACE != 804 & SWRdata$RACE != 805
                         & SWRdata$RACE != 806 & SWRdata$RACE != 807 & SWRdata$RACE != 808 & SWRdata$RACE != 809 
                         & SWRdata$RACE != 810 & SWRdata$RACE != 811 & SWRdata$RACE != 812 & SWRdata$RACE != 813
                         & SWRdata$RACE != 814& SWRdata$RACE != 815 & SWRdata$RACE != 816 & SWRdata$RACE !=817 & SWRdata$RACE != 818
                         & SWRdata$RACE !=819 & SWRdata$RACE !=820 & SWRdata$RACE != 830 & SWRdata$RACE != 999 & SWRdata$SEX != 2, ] 
Workstat_t <- t.test(clean_SWRdata$WKSTAT)
Workstat_t
clean_SWRdata$workstat <- NA
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 10] <- "1"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 11] <- "1"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 14] <- "1"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 15] <- "1"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 12] <- "2"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 20] <- "2"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 21] <- "2"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 22] <- "2"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 40] <- "2"
clean_SWRdata$workstat[clean_SWRdata$WKSTAT == 41] <- "2"

clean_SWRdata$race <- NA
clean_SWRdata$race[clean_SWRdata$RACE == 100] <- "White"
clean_SWRdata$race[clean_SWRdata$RACE == 200] <- "Black"
table(clean_SWRdata$RACE)
white <- clean_SWRdata[clean_SWRdata$race == "White", ]
black <- clean_SWRdata[clean_SWRdata$race == "Black", ]
nrow(white)
nrow(black)
mean(white$WKSTAT)
mean(black$WKSTAT)
mean(white$WKSTAT) - mean(black$WKSTAT)
t_sex_race <- t.test(black$WKSTAT, white$WKSTAT)
t_sex_race



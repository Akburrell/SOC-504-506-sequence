#Part 3 of the assignment
load("DATA_CPS_2016_2021.Rda")
race_hours <- CPS_2016_2021[ , c("RACE", "UHRSWORKT", "SEX", "VETSTAT")]
race <- race_hours[,"RACE"]
race_hours$race <- race_hours$RACE
race_hours$race <- NA
race_hours$race[race_hours$RACE == 100] <- "White"
race_hours$race[race_hours$RACE == 200] <- "Black"
hours <- race_hours[,"UHRSWORKT"]
race_hours$hours <- race_hours$UHRSWORKT
race_hours$hours[race_hours$UHRSWORKT  == "997"] <- NA
race_hours$hours[race_hours$UHRSWORKT  == "999"] <- NA
sex <- race_hours[,"SEX"]
race_hours$sex <- race_hours$SEX
race_hours$sex[race_hours$SEX  == "9"] <- NA
race_hours$VETSTAT[race_hours$VETSTAT  == "9"] <- NA
race_hours$VETSTAT[race_hours$VETSTAT  == "0"] <- NA
na.omit(race_hours)
clean_RH <- na.omit(race_hours) 

#how to include the intersectionality of race and gender in comparison to white males
clean_RH$WM <- ifelse(clean_RH$RACE == 100 & clean_RH$sex == 1,1,0)
clean_RH$WF <- ifelse(clean_RH$RACE == 100 & clean_RH$sex == 2,2,0)
clean_RH$BM <- ifelse(clean_RH$RACE == 200 & clean_RH$sex == 1,3,0)
clean_RH$BF <- ifelse(clean_RH$RACE == 200 & clean_RH$sex == 2,4,0)
clean_RH$VETSTAT <- ifelse(clean_RH$VETSTAT == 2,1,0)
table(clean_RH$WM + clean_RH$WF + clean_RH$BM + clean_RH$BF)

table(clean_RH$VETSTAT)
table(clean_RH$BF)
table(clean_RH$WF)
table(clean_RH$sex)
mean(clean_RH$UHRSWORKT)
#basic regression model that includes the intersectionality of race and gender
reg_results4 <- lm(clean_RH$UHRSWORKT ~ clean_RH$WF + clean_RH$BM + clean_RH$BF )
summary(reg_results4)
#EXCLUSIONARY MODEL - add potential sources of spuriousness/redundancy
reg_results5 <- lm(clean_RH$UHRSWORKT ~ clean_RH$WF + clean_RH$BM + clean_RH$BF  + clean_RH$VETSTAT)
summary(reg_results5)
median(clean_RH$UHRSWORKT)
mean(clean_RH$UHRSWORKT)
sd(clean_RH$UHRSWORKT)
table(clean_RH$VETSTAT)

#Part 2 of the assignment 
load(CPS_2016_2021_new)
incomechild<- CPS_2016_2021_new[, c("INCWAGE", "NCHILD")]
incomechild$INCWAGE[incomechild$INCWAGE == 99999999]<- NA
incomechild$INCWAGE[incomechild$INCWAGE == 99999998]<- NA
clean_incomechild <- na.omit(incomechild)
income_per_1k <- clean_incomechild[,"INCWAGE"]
clean_incomechild$income_per_1k <- clean_incomechild$INCWAGE
clean_incomechild$income_per_1k <- clean_incomechild$INCWAGE/1000
mean(income_per_1k)
reg_results <- lm(clean_incomechild$income_per_1k ~ clean_incomechild$NCHILD)
summary(reg_results)

#Question B
incomechildED<- CPS_2016_2021_new[, c("INCWAGE", "NCHILD", "EDUC")]
incomechildED$EDUC[incomechildED$EDUC == 000]<- NA
incomechildED$EDUC[incomechildED$EDUC == 001]<- NA
incomechildED$EDUC[incomechildED$EDUC == 999]<- NA
incomechildED$INCWAGE[incomechildED$INCWAGE == 99999999]<- NA
incomechildED$INCWAGE[incomechildED$INCWAGE == 99999998]<- NA
clean_incomechildED <- na.omit(incomechildED)
income_per_1k <- clean_incomechildED[,"INCWAGE"]
clean_incomechildED$income_per_1k <- clean_incomechildED$INCWAGE
clean_incomechildED$income_per_1k <- clean_incomechildED$INCWAGE/1000

#Question C
lessHS <- ifelse(clean_incomechildED$EDUC >= 002 & clean_incomechildED$EDUC <= 072,1,0)
HS <- ifelse(clean_incomechildED$EDUC == 073,2,0)
somecollege <- ifelse(clean_incomechildED$EDUC >= 080 & clean_incomechildED$EDUC <= 110,3,0)
BAmore <- ifelse(clean_incomechildED$EDUC >= 111 & clean_incomechildED$EDUC <= 125,4,0)
table(lessHS + HS + somecollege + BAmore)
reg_results2 <- lm(clean_incomechildED$income_per_1k ~ clean_incomechildED$NCHILD
                  + lessHS + HS + somecollege + BAmore)
summary(reg_results2)


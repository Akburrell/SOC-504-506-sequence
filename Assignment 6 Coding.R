load("DATA_CPS_2016_2021.Rda")
race_hours <- CPS_2016_2021[ , c("RACE", "UHRSWORKT", "SEX")]
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
na.omit(race_hours)
clean_RH <- na.omit(race_hours)

#PART TWO OF THE ASSIGNMENT
load("CPS_2016_2021_new.Rdata")
childincome <- CPS_2016_2021_new[ , c("INCWAGE", "NCHILD")]
clean_childinc <- childincome[childincome$INCWAGE != 99999999 & childincome$INCWAGE != 99999998, ]
incper1k <- clean_childinc[clean_childinc$INCWAGE/1000]
income_per_1k <- clean_childinc[,"INCWAGE"]
clean_childinc$income_per_1k <- clean_childinc$INCWAGE
clean_childinc$income_per_1k <- clean_childinc$INCWAGE/1000
mean(clean_childinc$income_per_1k)
mean(clean_childinc$NCHILD)
cor(clean_childinc$NCHILD, clean_childinc$income_per_1k, method="pearson", use = "complete.obs")
cor.test(clean_childinc$NCHILD,clean_childinc$income_per_1k, method="pearson")
reg_results <- lm(clean_childinc$NCHILD ~ clean_childinc$income_per_1k)
summary(reg_results)
ggscatter(clean_childinc, x = "NCHILD", y = "income_per_1k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Children", ylab = "Income/1k")






#PART 3 OF THE ASSIGNMENT
clean_RH$race <- ifelse(clean_RH$RACE == 200,1,0)
table(clean_RH$race)
clean_RH$sex <- ifelse(clean_RH$SEX == 2,1,0)
table(clean_RH$sex)
reg_results2 <- lm(clean_RH$UHRSWORKT ~  clean_RH$race)
reg_results3 <- lm(clean_RH$UHRSWORKT ~  clean_RH$sex)
summary(reg_results2)
summary(reg_results3)

#how to include the intersectionality of race and gender in comparison to white males
clean_RH$WM <- ifelse(clean_RH$RACE == 100 & clean_RH$sex == 1,1,0)
clean_RH$WF <- ifelse(clean_RH$RACE == 100 & clean_RH$sex == 2,2,0)
clean_RH$BM <- ifelse(clean_RH$RACE == 200 & clean_RH$sex == 1,3,0)
clean_RH$BF <- ifelse(clean_RH$RACE == 200 & clean_RH$sex == 2,4,0)
table(clean_RH$WM + clean_RH$WF + clean_RH$BM + clean_RH$BF)
table(clean_RH$BF)
table(clean_RH$WF)
table(clean_RH$sex)
reg_results4 <- lm(clean_RH$UHRSWORKT ~  clean_RH$WF + clean_RH$BM + clean_RH$BF)
summary(reg_results4)


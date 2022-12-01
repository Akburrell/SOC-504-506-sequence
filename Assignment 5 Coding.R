load("DATA_NSEE_36368-0001-Data.rda")
environmentA <- da36368.0001[ , c("BELIEVER_CAUSE", "DEMOG_GENDER")]
clean_environmentA <- environmentA[environmentA$BELIEVER_CAUSE != -9 & environmentA$BELIEVER_CAUSE != 98 & environmentA$BELIEVER_CAUSE != 99, ]
clean_environmentA <- environmentA[!is.na(environmentA$BELIEVER_CAUSE) & !is.na(environmentA$DEMOG_GENDER), ]
summary(clean_environmentA$BELIEVER_CAUSE)
summary(clean_environmentA$DEMOG_GENDER)
table(clean_environmentA$BELIEVER_CAUSE)  
table(clean_environmentA$DEMOG_GENDER,clean_environmentA$BELIEVER_CAUSE )
mytable <- table(clean_environmentA$DEMOG_GENDER,clean_environmentA$BELIEVER_CAUSE ); prop.table(mytable,2)
chisq.test(clean_environmentA$DEMOG_GENDER,clean_environmentA$BELIEVER_CAUSE, correct=FALSE)
# part 3 of the assignment

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
na.omit(race_hours)
clean_RH <- na.omit(race_hours)
mean(clean_RH$hours)

summary(clean_RH$hours)
clean_RH$hours <- clean_RH$UHRSWORKT
clean_RH$hours <- NA
clean_RH$hours[clean_RH$UHRSWORKT < 10] <- "Under 10 hours"
clean_RH$hours[clean_RH$UHRSWORKT >= 10 & clean_RH$UHRSWORKT < 20] <- "Few hours: 10-19.99"
clean_RH$hours[clean_RH$UHRSWORKT >= 20 & clean_RH$UHRSWORKT < 30] <- "Medium hours: 20-29.99"
clean_RH$hours[clean_RH$UHRSWORKT >= 30 & clean_RH$UHRSWORKT < 40] <- "Almost fulltime: 30-39.99"
clean_RH$hours[clean_RH$UHRSWORKT >= 40] <- "Full-time +"
table(clean_RH$hours)
table(clean_RH$hours, clean_RH$race)
mytable <- table(clean_RH$hours, clean_RH$race); prop.table(mytable,2)
chisq.test(clean_RH$hours, clean_RH$race, correct=FALSE)


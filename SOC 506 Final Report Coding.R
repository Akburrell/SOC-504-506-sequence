
install.packages("haven")
library(haven)
orginial<- read_sav("anes_timeseries_2016.sav")
election2016<- orginial[,c("V161188","V161496", "V167534", "V161310a", "V161310b", "V168307", "V168305")]
election2016<- na.omit(election2016)
election2016$new_black <- ifelse(election2016$V161310b == 1, 1, ifelse(election2016$V161310a == 1, 0, NA))
#count(unique(election2016$new_black))

#election2016[election2016$Ngun == "-9" & election2016$Ngun == "-5" & election2016$neighborhood == "-8" & election2016$neighborhood == "-1" &
                                  #election2016$age_by_group == "-6" & election2016$age_by_group == "-1" & election2016$black != "1" &
                                  #election2016$sex == "-6" & election2016$sex == "-1" & election2016$class != "1" & election2016$class != "2" & election2016$class != "3" & 
                                  #election2016$class != "4", ] <- NA
Ngun<- election2016[,"V161496"]
GunAcc<- election2016[,"V161188"]
neighborhood<- election2016[,"V167534"]
#age_by_group <- election2016[,"V168304"]
#black <- election2016[,"V161310b"]
sex <- election2016[,"V168307"]
#class<- election2016[,"V162132"]
#white <- clean_election2016[,"V161310a"]
election2016$GunAcc <- election2016$V161188
election2016$neighborhood <- election2016$V167534
#election2016$age_by_group <- election2016$V168304
#election2016$white <- election2016$V161310a
#election2016$black <- election2016$V161310b
election2016$sex <- election2016$V168307
#election2016$class <- election2016$V162132
election2016$Ngun <- election2016$V161496
table(election2016$Ngun)
table(election2016$neighborhood)
#table(election2016$age_by_group)
#table(election2016$black)
table(election2016$sex)
#table(election2016$class)
election2016$GunAcc[election2016$V161188 == -9] <- NA
election2016$GunAcc[election2016$V161188 == -8] <- NA
election2016$Ngun[election2016$V161496 == -9] <- NA
election2016$Ngun[election2016$V161496 == -5] <- NA
election2016$neighborhood[election2016$V167534 == -8] <- NA
election2016$neighborhood[election2016$V167534 == -1] <- NA
#election2016$age_by_group[election2016$V168304 == -1] <- NA
#election2016$age_by_group[election2016$V168304 == -6] <- NA
#create my own variable that include both black and white -- this is done above

election2016$sex[election2016$V168307 == -6] <- NA
election2016$sex[election2016$V168307 == -1] <- NA
#election2016$class[election2016$V162132 == -1] <- NA
#election2016$class[election2016$V162132 == -6] <- NA
#election2016$class[election2016$V162132 == -7] <- NA
#election2016$class[election2016$V162132 == -8] <- NA
#election2016$class[election2016$V162132 == -9] <- NA

#na.rm
#how do i change the categories from 1-4 to just 0 and 1

clean_2016<- na.omit(election2016)
summary(clean_2016)
summary(election2016)

neighborhood_factor <- factor(clean_2016$neighborhood, levels= c(1,2,3,4), labels= c("Farm","Rural","Suburban","Urban"))
sex_factor <- factor(clean_2016$sex, levels= c(1,2), labels= c("Male","Female"))
GunAcc_factor <- factor(clean_2016$GunAcc, levels= c(1,2,3,4,5), labels= c(" Extremely important", "Very important", "Somewhat important", "Not too important", "Not important at all"))

table(clean_2016$new_black)
table(neighborhood_factor)
table(sex_factor)
table(clean_2016$GunAcc)

library(MASS)
Guns<- polr(GunAcc_factor~1 + Ngun + new_black + neighborhood_factor, data = clean_2016)
summary(Guns)
exp(coef(Guns))
summary(Guns)$coefficients[,1]
summary(Guns)$standard.error[,2]
z<- summary(Guns)$coefficients[,1]/summary(Guns)$coefficients[,2]
z
pvalue<- (1-pnorm(abs(z),0,1)) *2
pvalue


boxplot(GunAcc_factor~Ngun, data = clean_2016 )
boxplot(GunAcc_factor~new_black, data = clean_2016 )






#We are transitioning to 2020 data

library(readr)
original2020 <- read_csv("anes_timeseries_2020_csv_20220210.csv")
View(election2020)
election2020 <- original2020[,c("V202355", "V203412", "V203413","V201628", "V202338" )]
election2020$new_black20 <- ifelse(election2020$V203412 == 2, 1, ifelse(election2020$V203412 == 1, 0, NA))

GunA <- election2020[,"V202338"]
Ngun<- election2020[,"V201628"]
neighborhood<- election2020[,"V202355"]
#age_by_group <- election2020[,"V203411"]
race <- election2020[,"V203412"]
sex <- election2020[,"V203413"]
#class<- election2020[,"V202352"]
election2020$GunA <- election2020$V202338
election2020$neighborhood <- election2020$V202355
#election2020$age_by_group <- election2020$V203411
election2020$race <- election2020$V203412
election2020$sex <- election2020$V203413
#election2020$class <- election2020$V202352
election2020$Ngun <- election2020$V201628

election2020$GunA[election2020$V202338 == -9] <- NA
election2020$GunA[election2020$V202338 == -7] <- NA
election2020$GunA[election2020$V202338 == -6] <- NA
election2020$GunA[election2020$V202338 == -5] <- NA
election2020$Ngun[election2020$V201628 == -9] <- NA
election2020$Ngun[election2020$V201628 == -5] <- NA
election2020$neighborhood[election2020$V202355 == -9] <- NA
election2020$neighborhood[election2020$V202355 == -8] <- NA
election2020$neighborhood[election2020$V202355 == -7] <- NA
election2020$neighborhood[election2020$V202355 == -6] <- NA
#election2020$age_by_group[election2020$V203411 == -1] <- NA
#election2020$age_by_group[election2020$V203411 == -2] <- NA
election2020$race[election2020$V203412 == -2] <- NA
election2020$race[election2020$V203412 == -1] <- NA
election2020$race[election2020$V203412 == 3] <- NA
election2020$sex[election2020$V203413 == -2] <- NA
election2020$sex[election2020$V203413 == -1] <- NA
#election2020$class[election2020$V202352 == -5] <- NA
#election2020$class[election2020$V202352 == -6] <- NA
#election2020$class[election2020$V202352 == -7] <- NA
#election2020$class[election2020$V202352 == -8] <- NA
#election2020$class[election2020$V202352 == -9] <- NA

clean20<- na.omit(election2020)

#setting up my factors
neighborhood_f20 <- factor(clean20$neighborhood, levels= c(1,2,3,4), labels= c("Rural","Small Town","Suburb","City"))
sex_f20 <- factor(clean20$sex, levels= c(1,2), labels= c("Male","Female"))
GunA_factor <- factor(clean20$GunA, levels= c(1,2,3,4,5), labels= c(" Extremely important", "Very important", "Somewhat important", "Not too important", "Not important at all"))

#Creating my Models 
Guns20<- polr(GunA_factor~1 + Ngun + new_black20 + neighborhood_f20, data = clean20)
summary(Guns20)

z<- summary(Guns20)$coefficients[,1]/summary(Guns20)$coefficients[,2]
z
pvalue<- (1-pnorm(abs(z),0,1)) *2
pvalue

boxplot(GunA_factor~Ngun, data = clean20 )
boxplot(GunA_factor~new_black20, data = clean20)

exp(coef(Guns20))
exp(coef(Guns))
(exp(coef(Guns20))) - (exp(coef(Guns)))

library(corrplot)
ggcorr(clean20[, c("GunA", "Ngun", "new_black20", "neighborhood")], geom = "circle", min_size = 10, max_size = 10,
       label = TRUE, label_alpha = TRUE, label_round = 2, label_size = 3,
       hjust = 1, layout.exp = 2)

library(corrplot)
ggcorr(clean_2016[, c("GunAcc", "Ngun", "new_black", "neighborhood")], geom = "circle", min_size = 10, max_size = 10,
       label = TRUE, label_alpha = TRUE, label_round = 2, label_size = 3,
       hjust = 1, layout.exp = 2)

library(GGally)
library(ggthemes)
ggpairs(clean_2016[, c("GunAcc", "Ngun", "new_black", "neighborhood")], lower = list(continuous = wrap("smooth", alpha = 0.5, size = 0.2))) +theme_tufte()

plot(Guns,which = c(1,2))
install.packages("sjPlot")
library(sjPlot)
plot_model(Guns)
#coef.plot -- look into it
#might need later 
white <- clean_election2016[,"V161310a"]
clean_election2016$white <- clean_election2016$V161310a
election2016[election2016$Ngun != -9 & election2016$Ngun != -5 & election2016$neighborhood != -8 & election2016$neighborhood != -1 &
               election2016$age_by_group != -6 & election2016$age_by_group != -1 & election2016$black == 1 &
               election2016$sex != -6 & election2016$sex != -1 & election2016$class == 1 & election2016$class == 2 & election2016$class == 3 & 
               election2016$class == 4, ]
Clean_2016<- na.omit(election2016$black & election2016$sex & election2016$neighborhood & election2016$age_by_group &
                       election2016$class & election2016$Ngun)

na.omit(election2016)
clean2016 <- na.omit(election2016)
attributes(clean2016$class)
library(tidyr)
library(dplyr)

election2016$GunAcc <- ifelse(election2016$V161188 == 1 &,1,0)
election2016$GunAcc <- ifelse(election2016$V161188 == 2,1,0)
election2016$GunAcc <- ifelse(election2016$V161188 == 3,1,0)

?as.factor
unique(election2016$GunAcc)
table(election2016$GunAcc)
Black <- glm(GunAcc~1 + Ngun + new_black + sex + (class==1) + (class==2) + (class==3)
                   + neighborhood + age_by_group, family= binomial(link='logit'), data = election2016)
summary(Black)

Black2020 <- lm(Ngun~1 + black20 + sex + I(class ==4) 
            + neighborhood + age_by_group, data = clean20)
summary(Black2020)


election2020$GunA[election2020$V202338 == 1] <- 1
election2020$GunA[election2020$V202338 == 2] <- 1
election2020$GunA[election2020$V202338 == 3] <- 1
election2020$GunA[election2020$V202338 == 4] <- 0
election2020$GunA[election2020$V202338 == 5] <- 0
White2020 <- lm(Ngun~1 + white20 + sex + class 
              + neighborhood + age_by_group, data = clean20)
summary(White2020)


election2016$GunAcc[election2016$V161188 == 1] <- 1
election2016$GunAcc[election2016$V161188 == 2] <- 1
election2016$GunAcc[election2016$V161188 == 3] <- 1
election2016$GunAcc[election2016$V161188 == 4] <- 0
election2016$GunAcc[election2016$V161188 == 5] <- 0



install.packages("haven")
library(haven)
orginial<- read_sav("anes_timeseries_2016.sav")
Belection2016<- orginial[,c("V161188","V161496", "V167534", "V161310b", "V168307", "V168305")]
Belection2016<- na.omit(Belection2016)
Ngun<- Belection2016[,"V161496"]
GunAcc<- Belection2016[,"V161188"]
neighborhood<- Belection2016[,"V167534"]
#age_by_group <- election2016[,"V168304"]
#black <- election2016[,"V161310b"]
sex <- Belection2016[,"V168307"]
#class<- election2016[,"V162132"]
#white <- clean_election2016[,"V161310a"]
Belection2016$GunAcc <- Belection2016$V161188
Belection2016$neighborhood <- Belection2016$V167534
#election2016$age_by_group <- election2016$V168304
#election2016$white <- election2016$V161310a
#election2016$black <- election2016$V161310b
Belection2016$sex <- Belection2016$V168307
#election2016$class <- election2016$V162132
Belection2016$Ngun <- Belection2016$V161496
#table(election2016$age_by_group)
#table(election2016$black)
#table(election2016$class)
Belection2016$GunAcc[Belection2016$V161188 == -9] <- NA
Belection2016$GunAcc[Belection2016$V161188 == -8] <- NA
Belection2016$Ngun[Belection2016$V161496 == -9] <- NA
Belection2016$Ngun[Belection2016$V161496 == -5] <- NA
Belection2016$neighborhood[Belection2016$V167534 == -8] <- NA
Belection2016$neighborhood[Belection2016$V167534 == -1] <- NA
#election2016$age_by_group[election2016$V168304 == -1] <- NA
#election2016$age_by_group[election2016$V168304 == -6] <- NA
#create my own variable that include both black and white -- this is done above

Belection2016$sex[Belection2016$V168307 == -6] <- NA
Belection2016$sex[Belection2016$V168307 == -1] <- NA
#election2016$class[election2016$V162132 == -1] <- NA
#election2016$class[election2016$V162132 == -6] <- NA
#election2016$class[election2016$V162132 == -7] <- NA
#election2016$class[election2016$V162132 == -8] <- NA
#election2016$class[election2016$V162132 == -9] <- NA

#na.rm
#how do i change the categories from 1-4 to just 0 and 1

Bclean_2016<- na.omit(Belection2016)
summary(Bclean_2016)
summary(Belection2016)
table(Bclean_2016$V161310b)
neighborhood_factor <- factor(Bclean_2016$neighborhood, levels= c(1,2,3,4), labels= c("Farm","Rural","Suburban","Urban"))
sex_factor <- factor(Bclean_2016$sex, levels= c(1,2), labels= c("Male","Female"))
GunAcc_factor <- factor(Bclean_2016$GunAcc, levels= c(1,2,3,4,5), labels= c(" Extremely important", "Very important", "Somewhat important", "Not too important", "Not important at all"))


library(MASS)
BGuns<- polr(GunAcc_factor~1 +V161310b+ Ngun + neighborhood_factor, data = Bclean_2016)
summary(BGuns)
exp(coef(BGuns))
summary(BGuns)$coefficients[,1]
summary(BGuns)$standard.error[,2]
Bz<- summary(BGuns)$coefficients[,1]/summary(BGuns)$coefficients[,2]
Bz
Bpvalue<- (1-pnorm(abs(Bz),0,1)) *2
Bpvalue


boxplot(GunAcc_factor~Ngun, data = Bclean_2016 )
boxplot(GunAcc_factor~new_black, data = clean_2016 )

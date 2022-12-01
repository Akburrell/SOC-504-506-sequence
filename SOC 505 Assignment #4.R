install.packages("MASS")
library(MASS)                                       
install.packages("olsrr")
library(olsrr)
install.packages("car")
library(car)
install.packages("boot")
library(boot)
#4.1a
#split contact needs to be split up into juvenial and adult 
#this is an option 
victimization <- glm(formula = victim~1 + agefirst + binge + as.factor(contact==2) + as.factor(contact==3) + currentage + sex, family = binomial(link = "logit"), data = victimization2022)
summary(victimization)
exp(coef(victimization))

#just testing the variables by themselves
victimbinge <- glm(victim~1 + sex + currentage + binge,
                   family=binomial(link="logit"),data=victimization2022)
summary(victimbinge)

victimagefirst <- glm(victim~1 + sex + currentage + agefirst,
                      family=binomial(link="logit"),data=victimization2022)
summary(victimagefirst)
victimcontact <- glm(victim~1 + sex + currentage + contact,
                     family=binomial(link="logit"),data=victimization2022)
summary(victimcontact)
exp(coef(victimization))
exp(coef(victimbinge))
exp(coef(victimagefirst))
exp(coef(victimcontact))

#4.1b
currentageV <- glm(victim~1 + currentage,
                      family=binomial(link="logit"),data=victimization2022)
agefirstV <- glm(victim~1 + agefirst,
                 family=binomial(link="logit"),data=victimization2022) 
summary(currentageV)
summary(agefirstV)
exp(coef(currentageV))
exp(coef(agefirstV))

#4.1c
library("ggplot2")
#Contact1
Model4.1c<- glm(victim~1 + binge + currentage + agefirst + sex + (contact==2) + (contact==3), family=binomial(link="logit"), data=victimization2022)
summary(Model4.1c)
contact1 <- with(Model4.1c, data.frame(agefirst=rep(seq(from=0, to= 21, length.out= 21), 1), currentage=25, binge=2, sex=1, contact=1))
Pcontact1 <- cbind(contact1, predict(Model4.1c, newdata=contact1, type="link", se=TRUE))
Pcontact1<-within(Pcontact1, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(Pcontact1, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_1<- ggplot(Pcontact1, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_1 + ggtitle("Contact Never")

#Contact2 
contact2 <- with(Model4.1c, data.frame(agefirst=rep(seq(from=0, to= 21, length.out= 21), 1), currentage=25, binge=2, sex=1, contact=2))
Pcontact2 <- cbind(contact2, predict(Model4.1c, newdata=contact2, type="link", se=TRUE))
Pcontact2<-within(Pcontact2, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(Pcontact2, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_2<- ggplot(Pcontact2, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_2 + ggtitle("Contact Juvenile")

#Contact 3
contact3 <- with(Model4.1c, data.frame(agefirst=rep(seq(from=0, to= 21, length.out= 21), 1), currentage=25, binge=2, sex=1, contact=3))
Pcontact3 <- cbind(contact3, predict(Model4.1c, newdata=contact3, type="link", se=TRUE))
Pcontact3<-within(Pcontact3, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(Pcontact3, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_3<- ggplot(Pcontact3, aes(x=agefirst, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)
Contact_3 + ggtitle("Contact Adult")

#4.1d
victimization2022$never <- ifelse(victimization2022$contact == 1,1,0)
victimization2022$any <- ifelse(victimization2022$contact == 2,1,0)
victimization2022$any <- ifelse(victimization2022$contact == 3,1,0)
Model4.1D<- glm(victim~1 + binge + currentage + agefirst + sex + never, family=binomial(link="logit"), data=victimization2022)
summary(Model4.1D)
Model4.1d<- glm(victim~1 + binge + currentage + agefirst + sex + any, family=binomial(link="logit"), data=victimization2022)
summary(Model4.1d)

#4.1E
victimizationlpm <- glm(formula = victim~1 + agefirst + binge + as.factor(contact==2) + as.factor(contact==3) + currentage + sex, family = gaussian(link = "identity"), data = victimization2022)
summary(victimizationlpm)

#4.1f
victimization2022$male <- ifelse(victimization2022$sex == 1,1,0)
victimization2022$female <- ifelse(victimization2022$sex == 0,1,0)

modelF_linear<- glm(victim~1 + agefirst + binge + I(contact==2) + I(contact==3) + currentage + sex + I(agefirst*sex), family=gaussian(link= "identity"), data = victimization2022)
summary(modelF_linear)
modelF_logit <- glm(victim~1 + agefirst + binge + I(contact==2) + I(contact==3) + currentage + sex + I(agefirst*sex), family = binomial(link = "logit"), data = victimization2022)
summary(modelF_logit)

#4.2 
#a 
Model4.2a <- glm(fees~1 + crt + minority + unemp +liberal, family= gaussian(link='identity'), data = courtfees2022)
summary(Model4.2a)
logLik(Model4.2a)
Model4.2acrt <- glm(fees~1 + crt, family= gaussian(link='identity'), data = courtfees2022)
summary(Model4.2acrt)
logLik(Model4.2acrt)
#b  just describe each varaible and its marginal effect on fees

#c
courtfees2022$feesres <- residuals(glm(fees~1 + minority + crt + unemp + liberal, family=gaussian(link= "identity"), data = courtfees2022))

courtfees2022$feeshat <- fitted(glm(fees~1 + minority + crt + unemp + liberal, family=gaussian(link= "identity"), data = courtfees2022))

ggplot(courtfees2022,aes(x=feeshat,y=feesres)) +
  geom_point()+
  geom_smooth(method="glm", formula=y~1)

#reject homoastatity-- had lower std error 
feesnull <- glm(feesres^2~1, family=gaussian(link=identity), data=courtfees2022)
fees3A <- glm(feesres^2~1 + feeshat, family=gaussian(link=identity), data=courtfees2022)

lmtest::lrtest(feesnull,fees3A)


library(boot)

bootstrap=function(modelA,index)
  return(coef(glm(fees~1 + minority + crt + unemp + liberal, family=gaussian(link= "identity"), data = courtfees2022, subset = index)))

boot(courtfees2022, bootstrap, 1000)


#d
courtfees2022$arenorm <- residuals(glm(formula = fees ~ 1 + crt + unemp + minority
                                       + liberal, family = gaussian(link = "identity"), data = courtfees2022))
qqnorm(courtfees2022$arenorm)
qqline(courtfees2022$arenorm)

#e
library(MASS)
library(olsrr)
#studentized residuals
courtfees2022$stres <- studres(Model4.2a)

ggplot(courtfees2022,aes(x=court,y=stres, label=court)) +
  geom_point() + 
  geom_text(vjust=1.1)

#cooks
courtfees2022$cook <- cooks.distance(Model4.2a)

ggplot(courtfees2022,aes(x=court,y=cook, label=court)) +
  geom_point() + 
  geom_text(vjust=1.1)

#evhat
courtfees2022$evhat<-hatvalues(Model4.2a)                               
ggplot(courtfees2022,aes(x=court,y=evhat, label=court)) +
  geom_point() + 
  geom_text(vjust=1.1)

#olsrr
evaloutls <- (lm(fees~1 + minority + crt + unemp + liberal, data = courtfees2022))
ols_plot_dfbetas(evaloutls)


#Removing influential cases
modelF<- glm(fees~1 + minority + crt + unemp + liberal, family=gaussian(link= "identity"), data = courtfees2022, subset = (court!=104 & court!=164 & court!=263 & court!=347))
summary(modelF)







#the things we didnt need
mean(victimization2022$currentage)
mean(victimization2022$binge)
victimbinge1 <- glm(victim~1 + sex + currentage + binge,
                    family=gaussian(link="identity"),data=victimization2022)
summary(victimbinge1)
LPM_binge<-with(victimbinge1, data.frame(binge=1:3, sex=1, currentage=30.4 ))  
LPM_binge<-predict(victimbinge1,newdata=LPM_binge, type="response", se=TRUE)
LPM_binge

victimagefirst1 <- glm(victim~1 + sex + currentage + agefirst,
                      family=gaussian(link="identity"),data=victimization2022)
summary(victimagefirst1)
LPM_agefirst<-with(victimagefirst1, data.frame(agefirst=rep(seq(from=0, to= 21, length.out= 21), 1), sex=1, currentage=30.4 ))  
LPM_agefirst<-predict(victimagefirst1,newdata=LPM_agefirst, type="response", se=TRUE)
LPM_agefirst

victimcontact1 <- glm(victim~1 + sex + currentage + contact,
                     family=gaussian(link="identity"),data=victimization2022)
summary(victimcontact1)
LPM_contact<-with(victimcontact1, data.frame(contact=rep(seq(from=0, to= 3, length.out= 3), 1), sex=1, currentage=30.4 ))  
LPM_contact<-predict(victimcontact1,newdata=LPM_contact, type="response", se=TRUE)
LPM_contact
#regular regression
Model4.1fmale <-  glm(victim~1 + male + agefirst,family=gaussian(link="identity"),data=victimization2022)
summary(Model4.1fmale)
Model4.1females <-  glm(victim~1 + female + agefirst,family=gaussian(link="identity"),data=victimization2022)
summary(Model4.1females)
#logit regression
Model4.1fmaleL <-  glm(victim~1 + male + agefirst, family=binomial(link="logit"),data=victimization2022)
summary(Model4.1fmaleL)
Model4.1femalesL <-  glm(victim~1 + female + agefirst, family=binomial(link="logit"),data=victimization2022)
summary(Model4.1femalesL)
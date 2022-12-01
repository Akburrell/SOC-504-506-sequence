#1.1
meanexpend<- glm(expend~1, family=gaussian(link="identity"), data=districtdata2021)
summary (meanexpend)  
logLik(meanexpend)
meanincome<- glm(income~1, family=gaussian(link="identity"), data=districtdata2021) 
summary (meanincome)
logLik(meanincome)
exincome<- lm(districtdata2021$expend ~ districtdata2021$income)
summary (exincome) 
logLik(exincome)
sd(districtdata2021$expend)
sd(districtdata2021$income)
median(districtdata2021$income)
median(districtdata2021$expend)

belowincome<- subset(districtdata2021, income<=40060)
aboveincome<- subset(districtdata2021, income>40060)

exincomeB <- glm(expend~1+ income, family=gaussian(link="identity"), data=belowincome)
exincomeA <- glm(expend~1 + income, family=gaussian(link="identity"), data=aboveincome)
summary(exincomeB)
logLik(exincomeB)
summary(exincomeA)
logLik(exincomeA)

#1.2
EIC <- lm(districtdata2021$expend~districtdata2021$income + districtdata2021$colgrad)
summary(EIC)
EICgeneral <- glm(formula= expend~1 + (income) + colgrad, family = gaussian(link = "identity"), data = districtdata2021)
summary(EICgeneral)
sd(districtdata2021$colgrad)
sd(districtdata2021$income)
sd(districtdata2021$expend)
meanincome<- glm(income~1, family=gaussian(link="identity"), data=districtdata2021) 
mean(districtdata2021$income)

#1.3
install.packages("psych")
library("psych")
install.packages("dplyr")
library("dplyr")
install.packages("reghelper")
library("reghelper")
EICmatrix<- cbind(districtdata2021$expend,districtdata2021$income,districtdata2021$colgrad)
describe(EICmatrix)
cov(EICmatrix)
cor(EICmatrix)
beta(EICgeneral, x=TRUE, y=TRUE)

#1.3

hopelessness <- lm(adolmentalhealth2021$hopeless~adolmentalhealth2021$fdistres + adolmentalhealth2021$age
                   + adolmentalhealth2021$cpbonds + adolmentalhealth2021$cabonds)
summary(hopelessness)
model1 <- glm(formula = hopeless~1 + age + fdistres, family=gaussian(link="identity"), data= adolmentalhealth2021)
summary(model1)
logLik(model1)
model2 <- glm(formula = hopeless~1 + age + cpbonds + cabonds + fdistres, family=gaussian(link="identity"), data= adolmentalhealth2021)
summary(model2)
logLik(model2)
beta(model2,x=TRUE, y=TRUE)

model3 <- glm(formula = hopeless~1 + fdistres, family=gaussian(link="identity"), data= adolmentalhealth2021)
summary(model3)

model4 <- glm(formula = resid(model3)~1 + age + cpbonds + cabonds + fdistres, family=gaussian(link="identity"), data= adolmentalhealth2021)
summary(model4)

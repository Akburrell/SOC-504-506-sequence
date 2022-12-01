install.packages("car") # for the linear hypotheses / constraints 
library("car")
install.packages("reghelper") # provides the beta ‐‐‐ creates standardized results
library("reghelper")
install.packages("lmtest") # likelihood ratio chi‐square and Wald chi‐square  test 
library("lmtest")
install.packages("ggplot2")  
library("ggplot2")
install.packages("psych")
library("psych")
#1.1
EE<- lm(org_research2021$ehours ~ org_research2021$admin + org_research2021$research + org_research2021$budget)
summary(EE)
confint(EE)
confint(EE, level=0.90)
BIC(EE)
modelb <-glm(ehours~1+ admin + research + budget + climate + hidegree + grants, family=gaussian(link="identity"), data=org_research2021)
summary(modelb)
BIC(modelb)
beta(modelb,x=TRUE, y=TRUE)

#1.2
#a
femcals <-glm(lef~1+ calories100s, family=gaussian(link="identity"), data=eatlifeprovinces2022)
summary(femcals)
BIC(femcals)
model1 <- glm(lef~1 + calories100s + flit, family=gaussian(link="identity"), data=eatlifeprovinces2022)
summary(model1)
coef(model1)
BIC(clinear)
#b
model2.1<-glm(lef~1 + calories100s + I(calories100s^2) + flit,family=gaussian(link="identity"),data=eatlifeprovinces2022)
summary(model2.1)
coef(model2.1)
cov(model2.1)
BIC(model2.1)
lmtest:: lrtest(model1, model2.1)

#plotting
ggplot(eatlifeprovinces2022,aes(x=calories100s*flit,y=lef)) +  
  geom_point() + 
  geom_smooth(method="glm", formula=y~poly(x,1)) 
ggplot(eatlifeprovinces2022,aes(x=calories100s*flit,y=lef)) +  
  geom_point()+ 
  geom_smooth(method="glm", formula=y~poly(x,2))

#interaction plot
ggplot(data=eatlifeprovinces2022) + 
  geom_point(aes(x=calories100s*flit,y=lef)) + 
  geom_smooth(aes(x=calories100s*flit, y= lef, method= 'gam'))

#interaction formula
model1_1 <- glm(lef~1 + calories100s*flit, family=gaussian(link="identity"), data=eatlifeprovinces2022)
summary(model1_1)

#c
meancals <- glm(calories100s~1, family=gaussian(link="identity"), data=eatlifeprovinces2022) 
summary(meancals)
sd(eatlifeprovinces2022$calories100s)

#d
eatlifeprovinces2022$caloriesmean <- (eatlifeprovinces2022$calories100s-25.8663)
eatlifeprovinces2022$flitmean <- (eatlifeprovinces2022$flit- 60.04972)
eatlifeprovinces2022$lefmean <- (eatlifeprovinces2022$lef- 67.25967)
MeanModel<- glm(formula=lefmean ~1 + caloriesmean + flitmean + I(caloriesmean^2), family = gaussian(link = "identity"), data = eatlifeprovinces2022)
summary(MeanModel)


#e
model3.1 <- glm(I(log(lef))~1 + I(log(calories100s)) + flit, family = gaussian(link = "identity"), data = eatlifeprovinces2022)
summary(model3.1)

#f
model4.1<- glm(I(log(lef))~1 + calories100s + flit, family = gaussian(link = "identity"), data = eatlifeprovinces2022)
summary(model4.1)

#g
logLik(model3.1)
logLik(model4.1)
logLik(model1)
logLik(model2.1)

#2.3
#a
modelA <- glm(penalties~1 + crt + unemp + liberal + minority + popsize, family = gaussian(link = "identity"), data = courtpenalties)
summary(modelA)
beta(modelA,x=TRUE, y=TRUE)
logLik(modelA)
#b
modelB <- glm(penalties~1 + crt + unemp + liberal + I(crt*minority) + popsize, family = gaussian(link = "identity"), data = courtpenalties)
summary(modelB)
logLik(modelB)
mean(courtpenalties$crt)
sd(courtpenalties$crt)
#c
modelC <- glm(penalties~1 + crt + unemp + I(crt*liberal) + minority + popsize, family = gaussian(link = "identity"), data = courtpenalties)
summary(modelC)
logLik(modelC)



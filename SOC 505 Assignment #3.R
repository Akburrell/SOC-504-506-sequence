#3.1A 
pccs <-glm(currentsevere~1 + priorcrtaction, family=gaussian(link="identity"),
           data=severity_pretrialdetainment)
summary(pccs)
coef(pccs)
BIC(pccs)
#B
ttest1<-glm(currentsevere~1 + I(priorcrtaction==1),
           family=gaussian(link="identity"),data=severity_pretrialdetainment)
summary(ttest1)
coef(ttest1)
BIC(ttest1)
#C
Model3c <-glm(currentsevere~1 + agecrime + priorsev + priornum + 
          I(schstatus==1) + I(schstatus==2)+ I(schstatus==3), family=gaussian(link="identity"),
          data=severity_pretrialdetainment)
summary(Model3c)
coef(Model3c)
BIC(Model3c)
#Null
meansevere <-glm(currentsevere~1, family=gaussian(link="identity"),
           data=severity_pretrialdetainment)
summary(meansevere)
BIC(meansevere)
#E
Model3e <-glm(currentsevere~1 + agecrime + priorsev + priorcrtaction + priornum + 
                I(schstatus==1) + I(schstatus==2) + I(schstatus==3), family=gaussian(link="identity"),
              data=severity_pretrialdetainment)
summary(Model3e)
coef(Model3e) # this is for F
BIC(Model3e)
logLik(Model3e)
logLik(Model3c)
#G
Model3g <-glm(currentsevere~1 +  priornum + priorcrtaction
              + agecrime + priorsev +I(schstatus==1) + 
                I(schstatus==2)+ I(schstatus==3)+ I(priornum*priorcrtaction), family=gaussian(link="identity"),
              data=severity_pretrialdetainment)
summary(Model3g)
coef(Model3g)
BIC(Model3g)
logLik(Model3g)
#H is a thought question
modelH<- glm(currentsevere~1 + agecrime + priornum + priorsev + I(schstatus==4) 
             + priorcrtaction, family = gaussian(link = "identity"), data = severity_pretrialdetainment)
summary(modelH)
#3.2A

Model3.2A <-glm(conflict~1 + schstress + alone + sex + delpeer +schactpeer, family=gaussian(link="identity"),
           data=familyconflict_peer2022)
summary(Model3.2A)
coef(Model3.2A)
BIC(Model3.2A)
logLik(Model3.2A)
#Null 
Model3.2AN <-glm(conflict~1 + schstress + alone + schactpeer, family=gaussian(link="identity"),
                data=familyconflict_peer2022)
summary(Model3.2AN)
BIC(Model3.2AN)
logLik(Model3.2AN)
#B
Model3.2B <- glm(conflict~1 + + schstress + alone + sex + delpeer +schactpeer + I(delpeer*sex), family=gaussian(link="identity"), data=familyconflict_peer2022)
summary(Model3.2B)
coef(Model3.2B)
BIC(Model3.2B)
logLik(Model3.2B)
#C thought question 
#every intercept would change if it were just male than if it were just female.

#3.4 A
library("ggplot2")
Model3.4A <- glm(clinical~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
summary(Model3.4A)
meanmale <- glm((sex==1)~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
meanfem <- glm((sex==0)~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
meanfam1 <- glm((famtype==1)~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
meanfam2 <- glm((famtype==2)~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
meanfam3 <- glm((famtype==3)~1, family=binomial(link="logit"), data=adolmentalhealth_clinical2020)

exp(coef(Model3.4A))
exp(coef(meanmale))
exp(coef(meanfem))
exp(coef(meanfam1))
exp(coef(meanfam2))
exp(coef(meanfam3))


Model3.4Aa <- glm(clinical~1 + I(sex==0), family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
summary(Model3.4Aa)
BIC(Model3.4Aa)

Model3.4Ab <- glm(clinical~1 + I(famtype==1) + I(famtype==2), family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
summary(Model3.4Ab)

#3.4B 
bivsexfam <- glm(formula = sex~1 +I(famtype==2) +I(famtype==3), family = binomial(link="logit"), data = adolmentalhealth_clinical2020) 
summary(bivsexfam)
coef(bivsexfam)
exp(coef(bivsexfam))

#3.4C
family <- as.factor(adolmentalhealth_clinical2020$famtype)
Model3.4C <- glm(clinical~1 + pconflict + cpeers + age + sex + I(family), family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
summary(Model3.4C)
coef(Model3.4C)
exp(coef(Model3.4C))
Model3.4Cc <- glm(clinical~1 + pconflict + cpeers + age + sex + (famtype==2)+ (famtype==3), family=binomial(link="logit"), data=adolmentalhealth_clinical2020)
summary(Model3.4Cc)
#run a model with and without family type and sex to determine if it matters then perform BIC

#3.4D 
females <- with(Model3.4Cc, data.frame(age=16, pconflict=5, cpeers=6, sex=0, famtype= 1))
males <- with(Model3.4Cc, data.frame(age=16, pconflict=5, cpeers=6, sex=1, famtype= 1)) 
femaleP <- predict(Model3.4Cc, newdata=females, type = "response")
maleP <-  predict(Model3.4Cc, newdata=males, type = "response")

fem_single <- with(Model3.4Cc, data.frame(age=16, pconflict=15, cpeers=0, sex=0, famtype= 2))
femsingleP <- predict(Model3.4Cc, newdata=fem_single, type="response")
male_single <- with(Model3.4Cc, data.frame(age=16, pconflict=15, cpeers=0, sex=1, famtype= 2)) 
malesingleP <- predict(Model3.4Cc, newdata=male_single, type="response")
#printed 
femaleP
maleP
femaleP-maleP
maleP-femaleP
femsingleP
malesingleP
malesingleP-femsingleP
#3.4E
#positive
FE <- with(Model3.4Cc, data.frame(cpeers=rep(seq(from=0, to= 6 , length.out= 7),2), age=18, pconflict=5, sex=0, famtype=1))
ME <- with(Model3.4Cc, data.frame(cpeers=rep(seq(from=0, to= 6 , length.out= 7),2),age=18, pconflict=5, sex=1, famtype=1))
PFE <- cbind(FE, predict(Model3.4Cc, newdata=FE, type="response", se=TRUE))
MPE <- cbind(ME,predict(Model3.4Cc, newdata=ME, type="response", se=TRUE))

Fprobs1o<-within(PFE, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(Fprobs1o, aes(x=cpeers, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)

Mprobs1o<-within(MPE, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(Mprobs1o, aes(x=cpeers, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)

#negative
negFE <- with(Model3.4Cc, data.frame(cpeers=rep(seq(from=0, to= 6 , length.out= 7),2), age=18, pconflict=20, sex=0, famtype=2))
negME <- with(Model3.4Cc, data.frame(cpeers=rep(seq(from=0, to= 6 , length.out= 7),2),age=18, pconflict=20, sex=1, famtype=2))
negPFE <- cbind(FE, predict(Model3.4Cc, newdata=negFE, type="response", se=TRUE))
negMPE <- cbind(ME,predict(Model3.4Cc, newdata=negME, type="response", se=TRUE))

negFprobs1o<-within(negPFE, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(negFprobs1o, aes(x=cpeers, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)

negMprobs1o<-within(negMPE, {PredictedProb<-plogis(fit)
LL<-plogis(fit - (1.96 * se.fit))
UL<-plogis(fit + (1.96 * se.fit))})
ggplot(negMprobs1o, aes(x=cpeers, y=PredictedProb)) + geom_line(aes(colour="red"), size=1)




#none of this is needed-- it was wrong
Model3.4D<- glm(clinical~1 + sex + age + famtype + pconflict + cpeers, family=binomial(link="logit"), data = adolmentalhealth_clinical2020)
summary(Model3.4D)
summary(adolmentalhealth_clinical2020$clinical)
adolmentalhealth_clinical2020$clinical
#sexdep<-with(clinical, data.frame((sex==1), (age==18), (famtype==2),  (pconflict==5), (cpeers==6)), data=adolmentalhealth_clinical2020)
summary(sexdep)

scene1<-predict(Model3.4D, newdata=sexdep,type="response")
scene1
#Not needed
Model3.4Ac <- glm(clinical~1 + I(sex==0) + I(famtype==1) + I(famtype==2), family=gaussian(link="identity"), data=adolmentalhealth_clinical2020)
summary(Model3.4Ac)
BIC(Model3.4Ac)
family <- as.factor()
test1 <- glm(clinical~1 + sex*famtype, family=gaussian(link="identity"), data=adolmentalhealth_clinical2020)
test2 <- glm(clinical~1 + I(sex== 0) + I(famtype==1) + I(famtype==2) + I(sex*famtype), family=gaussian(link="identity"), data=adolmentalhealth_clinical2020)
summary(test1)

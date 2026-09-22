part1<- read.table("C:/Users/Nora/OneDrive/Miamidatasets/PartI.csv", sep=",", header=TRUE)
part1

part1$HS= ifelse(part1$edu== 1,1,0)
table(part1$HS, part1$edu)
part1$college= ifelse(part1$edu==2,1,0)
table(part1$college,part1$edu)
part1$advance=ifelse(part1$edu==3,1,0)
table(part1$advance, part1$edu)

model= lm(lifesat~age+income+ses,data=part1)
summary(model)
confint(model, level=0.95)
lm.beta::lm.beta(model)
model2= lm(lifesat~age+income+ses+HS+college+advance, data=part1)
summary(model2)
confint(model2, level=0.95)
lm.beta::lm.beta(model2)
anova(model, model2)


edu= factor(part1$edu, 
           labels= c("didnt graduate hs","graduated","college","advance"))
edu

library("ggplot2")
plot.center_interaction<-ggplot(data=part1, aes(x=edu, y= lifesat, color= edu))+
  geom_point()+
  geom_smooth(aes(group=edu),method="lm", se= FALSE)+
  ylab("life satisfaction")+
  xlab("education level")+
  ggtitle("Life satisfaction based on education level")+
  theme_bw()
plot.center_interaction

model3= lm(lifesat~age+income+ses+college+advance, data=part1)
summary(model3)
confint(model3, level=0.95)
lm.beta::lm.beta(model3)

part1$reference <- ifelse(part1$edu %in% c( 1,2),"graduatedHS_ornot",
                          ifelse(part1$edu == 3, "college", "advance"))

reference = factor(part1$reference,
              labels = c("did + didn't graduate HS", "college", "advance"))
reference

library("ggplot2")
plot.center_interaction2 <- ggplot(data= part1, aes(x=income, y= lifesat, color= reference))+
  geom_point()+
  geom_smooth(aes(group=reference), method= "lm", se= FALSE)+
  ylab("Life satisfaction")+
  xlab("Income level")+
  ggtitle("Effect of income level on life satisfaction by edu level")+
  theme_bw()
plot.center_interaction2

model3= lm(lifesat~age+income*college + income*advance + ses, data=part1)
summary(model3)
confint(model3, level=0.95)
lm.beta::lm.beta(model3)

model4 =lm(lifesat~age+income+college+advance+psych+physical+spirit, data=part1)
summary(model4)
confint(model4,level=0.95)
lm.beta::lm.beta(model4)

anova(model3, model4)

part1$mstatus= ifelse(part1$married == 2,1,0)
table(part1$mstatus, part1$married)

options(scipen=999)
mreg= lm(lifesat~job*mstatus, data= part1)
summary(mreg)
confint(mreg, level=0.95)
lm.beta::lm.beta(mreg)

library("ggplot2")
plot.center_interaction3 <- ggplot(data= part1, aes(x= job, y= lifesat, color= mstatus))+
  geom_point()+
  geom_smooth(aes(group=mstatus), method= "lm", se= FALSE)+
  ylab("Life satisfaction")+
  xlab("Job Satisfaction")+
  ggtitle("Effect of job satisfaction on life satisfaction by marital status")+
  theme_bw()
plot.center_interaction3


library(aod)
library(psych)
library(DescTools)
library(car)
library(lmtest)

part1$smoke=as.factor(part1$smoke)
table(part1$smoke)
lreg = glm(smoke~gender+lifesat+job, data=part1, family="binomial")
summary(lreg)
exp(coef(lreg))
exp(cbind(OR=coef(lreg),confint(lreg)))
aod::wald.test(coef(lreg),Sigma=vcov(lreg), Terms = 1)
aod::wald.test(coef(lreg),Sigma=vcov(lreg), Terms = 2)
aod::wald.test(coef(lreg),Sigma=vcov(lreg), Terms = 3)
aod::wald.test(coef(lreg),Sigma=vcov(lreg), Terms = 4)
lreg$null.deviance
lreg$deviance
lreg$null.deviance-lreg$deviance
lreg$df.null-lreg$df.resisual
pchisq(lreg$null.deviance-lreg$deviance, (lreg$df.null-lreg$df.residual), lower.tail=FALSE)
DescTools::PseudoR2(lreg, which= 'all')
    



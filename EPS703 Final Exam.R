require(MVN)
require(car)
require(lattice)
require(mice)
require(lm.beta)
require(biotools)
require(multcomp)
require(emmeans)
options(scipen = 999)

partb <- read.table ("C:/Users/Nora/OneDrive/Miamidatasets/PartB.csv", sep = ",", quote = "\"'", header= TRUE)
partb

partb$Customer_type = factor(partb$Customer_type,
                                    levels = c("1", "2", "3"),
                                    labels = c("<1 year", "1-5 years",">5 years"))
partb$Region = factor(partb$Region,
                             levels = c("0", "1"),
                             labels = c ("USA", "Outside USA"))
str(partb)
table(partb$Customer_type, partb$Region)
attach(partb)

mvn(partb[, c("Satisfaction", "Support","Price")],
    mvnTest = "mardia")

partb$interaction <- as.factor(with(partb, interaction(Customer_type, Region), drop = TRUE))
biotools::boxM(partb[,c("Customer_type","Region")], partb$interaction)


fit.partb= manova(cbind(Satisfaction, Support, Price)~ Customer_type*Region, data = partb)
summary(fit.partb, type= "III", test= "Wilks")

post1.partb = aov(Satisfaction~Customer_type*Region, data= partb)
emmeans(post1.partb, list(pairwise~Customer_type|Region, pairwise~Region, pairwise~Customer_type), adjust= "Tukey")
post2.partb = aov(Support~Customer_type + Region + Customer_type*Region, data= partb)
emmeans(post2.partb, list(pairwise~Customer_type|Region, pairwise~Region, pairwise~Customer_type), adjust= "Tukey")
post3.partb= aov(Price~Customer_type + Region + Customer_type*Region , data= partb)
emmeans(post3.partb, list(pairwise~Customer_type|Region, pairwise~Region, pairwise~Customer_type), adjust = "Tukey")


mvn(partb[, c("Warranty", "Speed", "Quality")],
    mvnTest= "mardia")

fit1.partb= manova(cbind(Warranty, Speed, Quality)~Customer_type, data = partb)
summary(fit1.partb, type= "III", test= "Wilks")

post4.partb = aov(Warranty~Customer_type, data=partb)
emmeans(post4.partb, list(pairwise~Customer_type), adjust= "Tukey")
post5.partb= aov(Speed~Customer_type, data=partb)
emmeans(post5.partb, list(pairwise~Customer_type), adjust= "Tukey")
post6.partb= aov(Quality~Customer_type, data= partb)
emmeans(post6.partb, list(pairwise~Customer_type), adjust= "Tukey")


mvn(partb [, c("Satisfaction", "Purchase", "Recommend", "Price")],
    mvnTest= "mardia")

fit2.partb=manova(cbind(Satisfaction, Purchase, Recommend)~Region + Price, data= partb)
summary(fit2.partb, type="III", test= "Wilks")

post7.partb = aov(Satisfaction~Region + Price, data= partb)
emmeans(post7.partb, list(pairwise~Region), adjust = "Tukey")
post8.partb= aov(Purchase~Region + Price, data=partb)
emmeans(post8.partb, list(pairwise~Region), adjust= "Tukey")
post9.partb = aov(Recommend~Region + Price, data=partb)
emmeans(post9.partb, list(pairwise~Region), adjust= "Tukey")


#PART C

partc = read.table("C:/Users/Nora/OneDrive/Miamidatasets/PartC.csv", sep= ",", quote= "\"'", header= TRUE)
partc

mice::md.pattern(partc)
library(dplyr)
partc[partc == 999]<-NA
partc
mice::md.pattern(partc)
sum(is.na(partc))


partc_complete= partc[complete.cases(partc),]
partc_complete <- as.data.frame(partc_complete)

mvn(partc_complete[,c("V1","V2","V3","V4", "V5")],
    mvnTest= "mardia")

model1 = lm(partc[,c("age","V1", "V2", "V3", "V4", "V5")])
vif(model1)

tempdata = mice(partc, m= 222, maxit = 4, meth = 'pmm', seed = 500)
summary(tempdata)
completedata= complete(tempdata)
completedata

mreg= lm(V1~ V2 + V3 + V4, data= completedata)
summary(mreg)
confint(mreg, level = 0.95)
lm.beta::lm.beta(mreg)

# QUESTION 5

partc$prior = factor(partc$prior,
                     levels = c("1", "2"),
                     labels = c("High", "Low"))
partc$verbal = factor(partc$verbal,
                      levels = c("1", "2", "3"),
                      labels = c("Highly", "Moderate", "Low"))

str(partc)
table(partc$prior, partc$verbal)
attach(partc)

mvn(partc_complete[, c("V14", "V15", "V19")],
    mvnTest= "mardia")

model2 = lm(partc[,c("age", "V14", "V15", "V19")])
vif(model2)

partc$interaction <- interaction(partc$prior, partc$verbal, drop = TRUE)
biotools::boxM(partc[,c("V14", "V15","V19")], partc$interaction)

fit1.partc = manova(cbind(V14, V15, V19)~prior*verbal, data= partc)
summary(fit1.partc, type = "III", test = "Wilks")
summary.aov(fit1.partc)

post1.partc= aov(V14~prior*verbal, data= partc)
emmeans(post1.partc, list(pairwise~prior), adjust = "Tukey")
post2.partc= aov(V15~prior*verbal, data=partc)
emmeans(post2.partc, list(pairwise~prior), adjust= "Tukey")
post3.partc= aov(V19~prior*verbal, data=partc)
emmeans(post3.partc, list(pairwise~prior), adjust= "Tukey")


# QUESTION 6
library(emmeans)
library(biotools)
require(MASS)
require(CCA)
require(lda)
require(psych)
options(scipen= 999)


tempdata = mice(partc, m= 222, maxit = 4, meth = 'pmm', seed = 500)
summary(tempdata)
completedata= complete(tempdata)
completedata

partc.2 = completedata[,c("disability","V6","V7","V8","V9","V14","V16")]

partc.2$disability = factor(partc.2$disability,
                          levels= c("1", "2", "3", "4", "5", "6"),
                          labels= c("Dyslexia", "ADHD", "Language process disorder", "Dysgraphia", "Mixed", "Other"))
str(partc.2)

lda_analysis= MASS::lda(disability~ V6 + V7 + V8 + V9 + V14 + V16 , data= partc.2 )
summary(lda_analysis)
lda_analysis

fit= CCA::cc(as.matrix(partc.2[, c("V6", "V7","V8", "V9", "V14", "V16")]), as.matrix(as.numeric(partc.2$disability)))
fit$cor
fit$cor^2

partc.2 = cbind(partc.2, predict(lda_analysis)$x)
str(partc.2)

partc.2_DF1 = aov(LD1~disability, data= partc.2)
emmeans(partc.2_DF1, list(pairwise~disability), adjust= "Tukey")
partc.2_DF2 = aov(LD2~disability, data= partc.2)
emmeans(partc.2_DF2, list(pairwise~disability), adjust= "Tukey")
partc.2_DF3 = aov(LD3~disability, data= partc.2)
emmeans(partc.2_DF3, list(pairwise~disability), adjust= "Tukey")


Child1= as.data.frame(matrix(c(4, 6, 20, 3, 130, 59), nrow= 1, ncol= 6, byrow= TRUE))
colnames(Child1)= c("V6", "V7", "V8", "V9", "V14", "V16")
predict(lda_analysis, Child1)

Child2= as.data.frame(matrix(c(1, 4, 2, 2, 10, 15), nrow=1, ncol=6, byrow= TRUE))
colnames(Child2)= c("V6", "V7", "V8", "V9", "V14", "V16")
predict(lda_analysis, Child2)


# QUESTION 7 
require(nFactors)
require(foreign)
require(psych)
require(ggplot2)
options(scipen= 999)

partc.3 <- partc[,c("V1", "V2", "V3","V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21","V22", "V23", "V24")]
partc.3

psych::pairs.panels(partc.3, gap= 0, pch= 21)
psych::KMO(partc.3)
psych::cortest.bartlett(partc.3, n=nrow(partc.3))

cor.partc.3 = cor(partc.3)
eigen.values = eigen(cor.partc.3)$values
which(eigen.values >1)
plot(eigen.values)+abline(h = 1.0, col= "pink")
psych::fa.parallel(partc.3)

partc.3.rotate0 = psych::fa(partc.3, nfactors = 6, fm= "pa", rotate= "none")
partc.3.rotate0

partc.3.efa.rotate= psych::fa(partc.3, nfactors= 6, fm = "pa", rotate= "oblimin")
partc.3.efa.rotate

partc.3.efa.rotate1 = psych::fa(partc.3, nfactors=6, fm= "pa", rotate= "varimax")
partc.3.efa.rotate1

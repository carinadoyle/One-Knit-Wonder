rm (list = ls())
BMIdata <- read.table("PATH_W1.csv", header = TRUE, sep = ",")

library(Hmisc)
describe(BMIdata)

Path2 <- BMIdata
Path2$Race[Path2$Race==2] <- 0

Path2$Old <- factor(Path2$Old,labels = c("12-14 Years Old", "15-17 Years Old"))
Path2$Male <- factor(Path2$Male,labels = c("Male", "Female"))

table(Path2$Evertob, exclude = NULL)

EvertobTAB <- table(Path2$Evertob)
prop.table(EvertobTAB)

summary(Path2$BMI)
sd(Path2$BMI)
IQR(Path2$BMI)

aggregate(BMI ~ Evertob, Path2, mean)
aggregate(BMI ~ Evertob, Path2, sd)
aggregate(BMI ~ Evertob, Path2, median)
aggregate(BMI ~ Evertob, Path2, IQR)

table(Path2$Old)
OldTAB <- table(Path2$Old)
prop.table(OldTAB)

AgeByTobTAB <- table(Path2$Old, Path2$Evertob)
AgeByTobTAB
prop.table(AgeByTobTAB)

table(Path2$Male)
MaleTAB <- table(Path2$Male)
prop.table(MaleTAB)

GenderByTobTAB <- table(Path2$Male, Path2$Evertob)
GenderByTobTAB
prop.table(GenderByTobTAB)

table(Path2$Race)
RaceTAB <- table(Path2$Race)
prop.table(RaceTAB)

RaceByTobTAB <- table(Path2$Race, Path2$Evertob)
RaceByTobTAB
prop.table(RaceByTobTAB)

table(Path2$MHint)
MHintTAB <- table(Path2$MHint)
prop.table(MHintTAB)

MHintByTobTAB <- table(Path2$MHint, Path2$Evertob)
MHintByTobTAB
prop.table(MHintByTobTAB)

table(Path2$MHext)
MHextTAB <- table(Path2$MHext)
prop.table(MHextTAB)

MHextByTobTAB <- table(Path2$MHext, Path2$Evertob)
MHextByTobTAB
prop.table(MHextByTobTAB)

attach(Path2)
agelm <- lm(BMI~Old)
summary(agelm)
confint(agelm, 'Old', level=0.95)

evertoblm <- lm(BMI~Evertob)
summary(evertoblm)
confint(evertoblm, 'Evertob', level=0.95)

racelm <- lm(BMI~factor(Race))
summary(racelm)
confint(racelm, c('factor(Race)1','factor(Race)3', 'factor(Race)4') , level=0.95)

malelm <- lm(BMI~Male)
summary(malelm)
confint(malelm, 'Male', level=0.95)

intlm <- lm(BMI~MHint)
summary(intlm)
confint(intlm, 'MHint', level=0.95)

extlm <- lm(BMI~MHext)
summary(extlm)
confint(extlm, 'MHext', level=0.95)

detach(Path2)


### Model 1 ###
  
model1 <- lm(BMI ~ Evertob + Old + Male + factor(Race), data=Path2)
summary(model1)

#Assumption 1- Linearity 
##Since the IVs are categorical, there is no need to establish linearity between IV and DV. 

#Assumption 2 - Independence of residuals 
library(lmtest)
dwtest(model1)

#Assumption 3 - Normality 
par (mfrow = c (1,2)) #standardized residuals vs fitted plot
qqnorm(rstandard(model1), main = "Q-Q plot")
abline(0,1,col = "blue")
hist(rstandard(model1), main = " ")

plot(model1,which=2)

#Assumption 4 - Equal Variance
plot(model1, which = 3)

lmodResFit <- lm(sqrt(abs(residuals(model1))) ~ fitted(model1))
summary(lmodResFit)

#Assumption 5 - No Influential outliers
library(faraway)
cd <- cooks.distance(model1)
n <- nrow(Path2)
p <- 4
cd[which(abs(cd)>4/n)]
halfnorm(cd, main= "Halfnorm plot for Cook's Distance")

#Assumption 6 - No Strong Multicollinearity 
library(car)
car::vif(lm(BMI~Evertob+Old+Race, data=Path2)) #Assumption 6 not violated

### Model 2a ###

model2a <- lm(BMI ~ Evertob + Old + Male + factor(Race) + Evertob*Male, data=Path2)
summary(model2a)

anova(model1, model2a)

### Model 2b ###

model2b <- lm(BMI ~ Evertob + Old + Male + factor(Race) + Evertob*Old, data=Path2)
summary(model2b)

anova(model1, model2b)

### Model 3 ###

model3a <- lm(BMI ~ Evertob + Old + Male + factor(Race) + Evertob + MHint + MHext, data=Path2)
summary(model3a)

model3b <- lm(BMI ~ Evertob + Old + Male + factor(Race) + Evertob*Male + MHint*Male + MHext*Male, data=Path2)
summary(model3b)

anova(model3a, model3b)


claimants<- read.csv(file.choose())
View(claimants)
library(Amelia)
missmap(claimants)
summary(claimants)
str(claimants)
summary(claimants)
attach(claimants)
claimants$ATTORNEY = as.factor(claimants$ATTORNEY)
claimants$SEATBELT = as.factor(claimants$SEATBELT)
summary(claimants$SEATBELT)
claimants1 <-na.omit(claimants)
summary(claimants1)
1340-1096
244/1340
library(dummies)
dummies = dummy(claimants$CLMINSUR, sep = "_")
View(dummies)
#imputations -  #Deleting #Central tendency #regression
library(Hmisc)
claimants$CLMAGE <- with(claimants, impute(claimants$CLMAGE,mean))
?impute
?with
summary(claimants)

#Splitting data into Train & Test : 70 & 30

  # Logistic Regression

logit=glm(ATTORNEY ~ factor(CLMSEX) + factor(CLMINSUR) + factor(SEATBELT) 
          + CLMAGE + LOSS,family= "binomial",data=claimants)
summary(logit)


# Odds Ratio


# Confusion Matrix Table

prob=predict(logit,type=c("response"),claimants)
View(prob)

confusion<-table(prob>0.5,claimants$ATTORNEY)
confusion


# Model Accuracy

Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy


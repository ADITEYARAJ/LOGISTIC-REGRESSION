bank <- read.csv(file.choose())
View(bank)
library(Amelia)
missmap(bank)
str(bank)
df <- as.data.frame(bank)
#pre prepocessing the data
#convert data given in one column to 17 columns
df1 <- as.character(df[,])
bank1 <- strsplit(df1,split =";")
bank1
bank2 <- matrix(unlist(bank1),nrow=45211,byrow = T)
View(bank2)
colnames(bank2) <- c("age","job","marital","education","default","balance","housing","loan","contact","day","month","duration","campaign","pdays","previous","poutcome","y")
View(bank2)
bank3 <- as.data.frame(bank2)
str(bank3)
summary(bank3)
attach(bank3)
View(bank3)
model <- glm(y~.,data = bank3)
summary(model)

# Confusion Matrix Table
prd <- predict(model,type=c("response"),test,family="binomial")
View(prd)

confusion <- table(prd >0.5,bank3$y)
confusion

#model accuracy
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prd,y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained


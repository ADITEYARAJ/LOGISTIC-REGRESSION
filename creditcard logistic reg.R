card <- read.csv(file.choose())
View(card)
str(card)
summary(card)
attach(card)
model <- glm(card~.,data = card,family = "binomial")
summary(model)#AIC=11632
#model selection by using the least AIC value
model.null <- glm(card~1,data = card,family = "binomial")
model.full <- glm(card~.,data = card,family = "binomial")
#Episode I - forward selection
step(model.null,scope = list(upper=model.full),direction = "forward")
#Episode II - backward elimination
step(model.full,scope=list(lower=model.null,direction="backward"))
#Episode III - bidirectional elimination
model.card <- glm(card~active+age+dependents+expenditure,data = card,family = "binomial")
step(model.card,scope = list(upper=model.full,lower=model.null),direction = "both")
# active + age + dependents + expenditure + reports + majorcards are the best variable for prediction 
#with an AIC scare of 128.5
model.final <- glm(formula = card ~ active + age + dependents + expenditure + 
                     reports + majorcards, family = "binomial", data = card)
summary(model.final)
# Confusion Matrix Table
prd <- predict(model.final,type=c("response"),data=test,family="binomial")
prd
length(prd)
confusion <- table(prd>0.5,card$card)
confusion

#model accuracy
accuracy <- sum(diag(confusion))/sum(confusion)
accuracy

# ROC Curve 
library(ROCR)
rocrpred<-prediction(prd,card$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))
# More area under the ROC Curve better is the logistic regression model obtained

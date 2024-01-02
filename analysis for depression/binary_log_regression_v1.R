# STAT 228 Final Project
# Binary Logistic Regression Model
# 4/12/23

c <- read.csv(file.choose()) #import "clean_vars.csv"
dim(c)
names(c)

#without depression_total_score
cleaned <- c[,-26]
cleaned$depress_binary <- as.factor(cleaned$depress_binary)

# -------------- model 1: train and test sets ---------------
# -------------- a) create train and test sets --------------
set.seed(1)
train.index <- sample(1:nrow(cleaned), size=0.7*nrow(cleaned), replace=FALSE) 
train.set <- cleaned[train.index,]
test.set <- cleaned[-train.index,]

# -------------- b) fit binary logistic regression model --------
log1 <- glm(depress_binary~., family=binomial(link="logit"), data=train.set) #WARNING MESSAGE
phat.log1 <- predict(log1, newdata=test.set, type="response")
yhat.log1 <- ifelse(phat.log1 > 0.5, 1, 0) #discriminating threshold of 0.5
  #if 1, it is predicted to be 1; if 0, it is predicted to be 0
conf.log1 <- table(yhat.log1, test.set$depress_binary); conf.log1
# confusion matrix
# yhat.log1  0  1 
#         0 26  4
#         1  3  3

# -------------- c) calculate performance metrics ---------------
#misclassification rate, sensitivity (true positive), specificity (true negative) (in order)
mis.log1 <- (conf.log1[1,2]+conf.log1[2,1])/sum(conf.log1) 
sens.log1 <- conf.log1[2,2]/sum(conf.log1[,2]) 
spec.log1 <- conf.log1[1,1]/sum(conf.log1[,1]) 

#ROC curve, AUC
library("pROC")
plot(roc(test.set$depress_binary, phat.log1))
auc(test.set$depress_binary, phat.log1) # Area under the curve: 0.6552

#4 diagnostic plots => WARNING MESSAGE
plot(log1)
# outlying and influential observations: 24,28,23,2,79,85
# CONFIRM/QUESTION: this is only for train set (bc it's based off of log1 which only uses train set) right?


# -------------- model 2: remove outlying/influential obs ------------------
# -------------- a) create train and test sets --------------
train.set2 <- train.set[-c(2,23,24,28,79,85),]
test.set2 <- test.set

# -------------- b) fit binary logistic regression model --------
log2 <- glm(depress_binary~., family=binomial(link="logit"), data=train.set2) #WARNING MESSAGE
phat.log2 <- predict(log2, newdata=test.set2, type="response")
yhat.log2 <- ifelse(phat.log2 > 0.5, 1, 0) #discriminating threshold of 0.5
conf.log2 <- table(yhat.log2, test.set2$depress_binary); conf.log2
# confusion matrix
# yhat.log2  0  1
#         0 27  4
#         1  2  3

# -------------- c) calculate performance metrics ---------------
#misclassification rate, sensitivity, specificity (in order)
mis.log2 <- (conf.log2[1,2]+conf.log2[2,1])/sum(conf.log2) # [1] 0.1666667
sens.log2 <- conf.log2[2,2]/sum(conf.log2[,2]) # [1] 0.4285714
spec.log2 <- conf.log2[1,1]/sum(conf.log2[,1]) # [1] 0.9310345

#ROC curve, AUC
library("pROC")
plot(roc(test.set2$depress_binary, phat.log2))
auc(test.set2$depress_binary, phat.log2) # Area under the curve: 0.7611

#4 diagnostic plots => WARNING MESSAGE
plot(log2)
#still get out 24, 28, 23, 2, 57, 85?
#need to remove from entire data set?

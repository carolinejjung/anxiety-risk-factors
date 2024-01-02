# STAT 228 Final Project
# SVM

c <- read.csv(file.choose()) #import "clean_vars.csv"
dim(c)
names(c)

#without depression_total_score
cleaned <- c[,-26]

library("e1071")
set.seed(1) 
train.index <- sample(1:nrow(cleaned), size=0.7*nrow(cleaned), replace=FALSE) 
train.set <- cleaned[train.index,] #same train.set from binary_log_regression_v1 file
test.set <- cleaned[-train.index,]

cleaned$depress_binary <- as.factor(cleaned$depress_binary)

#-----------------SVM with linear kernel----------------
svm.linear1 <- svm(depress_binary~., data=train.set, type="C-classification", kernel="linear", cost=1)
yhat.svm.linear1 <- predict(svm.linear1, newdata=test.set) #error?
conf.svm.linear1 <- table(yhat.svm.linear1, test.set$depress_binary) #confusion matrix
mis.svm.linear1 <- (conf.svm.linear1[1,2]+conf.svm.linear1[2,1])/sum(conf.svm.linear1)

#----------------SVM with polynomial kernel of degree 3---------
svm.poly1 <- svm(depress_binary~., data=train.set, type="C-classification", kernel="polynomial", degree=3, cost=1)
yhat.svm.poly1 <- predict(svm.poly1, newdata=test.set) #error?
conf.svm.poly1 <- table(yhat.svm.poly1, test.set$depress_binary) #confusion matrix
mis.svm.poly1 <- (conf.svm.poly1[1,2]+conf.svm.poly1[2,1])/sum(conf.svm.poly1)

#----------------SVM with radial kernel with gamma 0.1----------
svm.rad1 <- svm(depress_binary~., data=train.set, type="C-classification", kernel="radial", gamma=0.1, cost=1)
yhat.svm.rad1 <- predict(svm.rad1, newdata=test.set) #error?
conf.svm.rad1 <- table(yhat.svm.rad1, test.set$depress_binary) #confusion matrix
mis.svm.rad1 <- (conf.svm.rad1[1,2]+conf.svm.rad1[2,1])/sum(conf.svm.rad1)

#ERRORS



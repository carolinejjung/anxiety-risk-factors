# STAT 228
# ensemble methods - bagging and random forest

health <- read.csv("selected_vars_v5.csv")

library(randomForest)
set.seed(1)

choose.ntree.bag <- randomForest(anxiety_perception~.,data=health,ntree=500,mtry=4) 
plot(choose.ntree.bag) # lowest around ntree = 100

choose.ntree.rf <- randomForest(anxiety_perception~.,data=health,ntree=500,mtry=4) 
plot(choose.ntree.rf) # lowest around ntree = 100

?randomForest

# 5-fold cross validation

n <- dim(health)[1]
K <- 5 # since we are performing 5-fold CV
n.fold <- round(n/K) # size of each fold = 6

shuffle <- sample(1:n, n, replace=FALSE)
index.fold <- list() # create list of random subsets
for (i in 1:K) {
	if (i < K) {
		index.fold[[i]] <- shuffle[((i-1) * n.fold + 1) : (i * n.fold)]
	} else {
		index.fold[[i]] <- shuffle[((K-1) * n.fold + 1) : n]
	}
}

index.fold

CV.score.bag <- 0
for(i in 1:K) {
	# fit on data except ith fold
	fit.bag <- randomForest(anxiety_perception~.,data=health[-index.fold[[i]],],ntree=100,mtry=4) 
	# predict for ith fold
	Yhat.bag <- predict(fit.bag,newdata=health[index.fold[[i]],])
	# error result
	CV.score.bag <- CV.score.bag + (1/n) * sum((health$anxiety_perception[index.fold[[i]]] - Yhat.bag)^2)
}

CV.score.bag # 6.132598

# random forest
CV.score.rf <- 0
for(i in 1:K) {
	# fit on data except ith fold
	fit.rf <- randomForest(anxiety_perception~.,data=health[-index.fold[[i]],],ntree=100,mtry=2) 
	# predict for ith fold
	Yhat.rf <- predict(fit.rf,newdata=health[index.fold[[i]],])
	# error result
	CV.score.rf <- CV.score.rf + (1/n) * sum((health$anxiety_perception[index.fold[[i]]] - Yhat.rf)^2)
}

CV.score.rf # 6.282022

#VISUALIZING PREDICTORS

importance(choose.ntree.bag)
importance(choose.ntree.rf)





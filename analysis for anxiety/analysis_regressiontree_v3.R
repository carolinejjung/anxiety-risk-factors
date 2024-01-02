clean <- read.csv(file.choose()) #selected_vars_v5

# MULTIPLE LINEAR REGRESSION
# full model
lm.full <- lm(anxiety_perception~., data=clean)
summary(lm.full)

# regression diagnostics for residuals
e <- resid(lm.full) #residuals
plot(fitted(lm.full), e, main="Residual Plot", xlab="Fitted Values", ylab="Residuals") #residual plot - linear, mean 0, constant variance
qqnorm(e, main="Q-Q plot for Residuals"); qqline(e) #approx normal

# regression diagnostics for anxiety scores
plot(fitted(lm.full), clean$anxiety_perception, xlab="Fitted Values", ylab="Anxiety Scores")
hist(clean$anxiety_perception, xlab="Anxiety Scores", main="Histogram of Anxiety Scores") #not normal
qqnorm(clean$anxiety_perception, main="Q-Q plot for Anxiety Scores"); qqline(clean$anxiety_perception)

# box cox: finding optimal lambda
# do this bc constant variance assumption isn't met
library(MASS)
clean$anxiety_perception[clean$anxiety_perception==0] <- 0.01 #need positive value
par(mfrow=c(1,1))
boxcox(lm.full) #optimal lambda = 0.5
lmbox <- lm(anxiety_perception^(1/2)~., data=clean)
scatter.smooth(residuals(lmbox)~predict(lmbox))
lmbox$coefficients

# box cox transformation
clean$anxiety_perception <- clean$anxiety_perception^(0.5)

# REGRESSION TREE
library("tree")

# FULL TREE
tree.full <- tree(anxiety_perception~., data=clean)
plot(tree.full); text(tree.full, pretty=0); title("Regression Tree")

# PRUNED TREE
#finding optimal size by running it multiple times
test <- c()
for(i in 1:1000){
  result <- cv.tree(tree.full, K=10, FUN=prune.tree) #based on 10 fold CV
  index <- which(min(result$dev)==result$dev)
  test[i] <- result$size[index]
}
median(test); mean(test)
hist(test, xlab="Optimal Sizes", main="Possible Optimal Sizes for 
     Pruned Tree (lowest deviance)") #5 is the mode

#find pruned tree
tree.pruned <- prune.tree(tree.full, best=5)
plot(tree.pruned); text(tree.pruned, pretty=0)


# 5 FOLD CV FOR REGRESSION TREE - full
n <- nrow(clean) #sample size
k <- 5 #5 subsets
n.fold <- floor(n/k) #size of each fold, rounded down
set.seed(1)
nshuffle <- sample(1:n, n, replace=FALSE) #shuffles the n indexes
indexfold <- list()
for(i in 1:k){
  if(i<k){
    indexfold[[i]] <- nshuffle[((i-1)*n.fold+1):(i*n.fold)]
  }else{
    indexfold[[i]] <- nshuffle[((i-1)*n.fold+1):n]
  }
}

cvscore <- 0
for(i in 1:k){
  #fit model excluding ith fold
  tree.full2 <- tree(anxiety_perception~., data=clean[-indexfold[[i]],])
  #make prediction on each observation in the ith fold
  pred <- predict(tree.full2, clean[indexfold[[i]],])
  #compute avg squared error for ith fold
  cvscore <- cvscore+(1/n)*sum((clean$anxiety_perception[indexfold[[i]]]-pred)^2)
}
cvscore

# 5 FOLD CV FOR PRUNED TREE
n <- nrow(clean) #sample size
k <- 5 #5 subsets
n.fold <- floor(n/k) #size of each fold, rounded down
set.seed(1)
nshuffle <- sample(1:n, n, replace=FALSE) #shuffles the n indexes
indexfold <- list()
for(i in 1:k){
  if(i<k){
    indexfold[[i]] <- nshuffle[((i-1)*n.fold+1):(i*n.fold)]
  }else{
    indexfold[[i]] <- nshuffle[((i-1)*n.fold+1):n]
  }
}

cvscore2 <- 0
for(i in 1:k){
  #fit model excluding ith fold
  tree.full3 <- tree(anxiety_perception~., data=clean[-indexfold[[i]],])
  tree.pruned2 <- prune.tree(tree.full3, best=5)
  #make prediction on each observation in the ith fold
  pred <- predict(tree.pruned2, clean[indexfold[[i]],])
  #compute avg squared error for ith fold
  cvscore2 <- cvscore2+(1/n)*sum((clean$anxiety_perception[indexfold[[i]]]-pred)^2)
}
cvscore2


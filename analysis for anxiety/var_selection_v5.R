# STAT 228 final project
# variable selection
# 4/26/2023

health <- read.csv("clean_v4.csv")
dim(health)
names(health)

# ---------------------------------- #
# remove non-predictor variables
health_preds <- health[,-c(1, 2, 5)]
names(health_preds)

# ---------------------------------- #
# lasso regression for variable selection
library(glmnet)

y <- health_preds$anxiety_perception
x <- health_preds[,-c(which(names(health_preds) == "anxiety_perception"))]

set.seed(1)
result.lasso <- cv.glmnet(x=data.matrix(x),y=y,family="gaussian",alpha=1)
coefs.lasso <- coef(result.lasso,s=result.lasso$lambda.min)
coefs.lasso

# ---------------------------------- #
# vifstep to check multicollinearity
# among variables selected by lasso regression
library(usdm)

preds <- c("age","screening_score","depression_total_score",
          "social_visits","social_phone","comorbidities_count","medication_count")
numeric <- x[,which(names(x)%in%preds)]
vifstep(numeric)

# No variable from the 7 input variables has collinearity problem.

# ---------------------------------- #
# Making new csv file for export
coef2 <- c("anxiety_perception", "age", "comorbidities_most_important", "screening_score", "depression_total_score", "social_visits", "social_phone", "health_rate", "comorbidities_count", "medication_count")
selected <- health_preds[,which(names(health_preds)%in%coef2)]

write.csv(health_preds, "selected_vars_v4.csv", row.names=FALSE)
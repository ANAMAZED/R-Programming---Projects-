#Install lars package first 
# used tools to install
installed.packages("lars")

# Load the diabetes data
library(lars)
data(diabetes)
names(diabetes)

# To load entire dataset for diabetes:
data.all<-data.frame(cbind(diabetes$x,y=diabetes$y))
# eXPLORING THE DATA 
summary(data.all)
str(data.all)   
anyNA(data.all) 

# Partition the patients into two groups: training(75%) and test (25%)
n <-dim(data.all)[1]            # samplesize=442 
set.seed(1306)


# Partition the patients into two groups: training (75%) and test (25%)
n <- dim(data.all)[1] # sample size = 442
set.seed(1306) # set random number generator seed to enable
# repeatability of results
test <- sample(n, round(n/4)) # randomly sample 25% test
data.train <- data.all[-test,]
data.test <- data.all[test,]
x <- model.matrix(y ~ ., data = data.all)[,-1] # define predictor matrix
str(x)
summary(x)


# excl intercept col of 1s
x.train <- x[-test,] # define training predictor matrix
x.test <- x[test,] # define test predictor matrix
y <- data.all$y # define response variable
y.train <- y[-test] # define training response variable
y.test <- y[test] # define test response variable
n.train <- dim(data.train)[1] # training sample size = 332
n.test <- dim(data.test)[1] # test sample size = 110

pairs(data.train)
pairs(data.test)

#Install packages and load libraries 
install.packages("tidyverse")
install.packages("ISLR")
install.packages("MASS")
install.packages("dplyr")
install.packages("GGally")
install.packages("ggplot2")
install.packages("corrplot")
#Load libraries
library(tidyverse) 
library(MASS) 
library(ISLR)
library(DPLYR)
library(ggplot2)
library(GGally)
library(corrplot)

ggpairs(data.train)
ggpairs(data.test)

library(Hmisc)
r <- rcorr(as.matrix(data.train))
corrplot(r$r, type='upper',method = "shade", shade.col = NA, p.mat=r$P,
         tl.col="black", tl.srt = 45,number.cex = 1,addCoef.col = 'blue',
         order='hclust',sig.level = 0.05, insig = c("pch"), diag = FALSE,
         col=colorRampPalette(c("orange","yellow","green"))(200))

hist(y.train,
     main="Histogram for Response variable", 
     xlab="Response Variable - y", ylab= "Frequency",
     border="blue",
     col="lightblue")

boxplot(y.train, data=data.all, main="Box plot of Response Variable", col="green")

par(mfrow = c(3, 3))
hist(data.train$age)
hist(data.train$bmi)
hist(data.train$map)
hist(data.train$tc)
hist(data.train$ldl)
hist(data.train$hdl)
hist(data.train$tch)
hist(data.train$ltg)
hist(data.train$glu)

par(mfrow = c(3,3))
boxplot(data.train$age, data=data.all, main="Box plot of data.train$age", col="grey")
boxplot(data.train$bmi, data=data.all, main="Box plot of data.train$bmi", col="grey")
boxplot(data.train$map, data=data.all, main="Box plot of data.train$map", col="grey")
boxplot(data.train$tc, data=data.all, main="Box plot of data.train$tc", col="yellow")
boxplot(data.train$ldl, data=data.all, main="Box plot of data.train$ldl", col="yellow")
boxplot(data.train$hdl, data=data.all, main="Box plot of data.train$hdl", col="yellow")
boxplot(data.train$glu, data = data.all, main = "Boxplot of data.train$glu", col = "blue")
boxplot(data.train$ltg, data = data.all, main = "Boxplot of data.train$ltg", col = "blue")
boxplot(data.train$tch, data = data.all, main = "Boxplot of data.train$tch", col = "blue")


#Linear Model - Training data 
Model_lm = lm(y~. , data = data.train)
summary(Model_lm)
coef(Model_lm)
par(mfrow = c(2,2))
plot(Model_lm)


y.test
pred = predict(Model_lm, data.test)
mean((data.test$y - predict(Model_lm, data.test))^2) 
sd((y.test - pred)^2)/sqrt(n.test) 

 

# BEST SUBSET SELECTION - USING BIC 
library(leaps)
Model_bic <- regsubsets(y ~ ., data = data.train, nvmax = 10)
summary(Model_bic)
par(mfrow = c(1, 2))
plot(Model_bic, scale = "bic", main = "Predictor Variables vs. BIC")
summary(Model_bic)
summary(Model_bic)$bic
plot(summary(Model_bic)$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l",
     main = "Best Subset Selection Using BIC")
which.min(summary(Model_bic)$bic)
points(5, summary(Model_bic)$bic[5], col = "green", cex = 2, pch = 20)
coef(Model_bic, 5)

predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  cof <- coef(object, id = id)
  xvars <- names(cof)
  mat[, xvars]%*%cof
}

mean((y.test - predict(Model_bic, data.test, id = 5))^2) 
sd((y.test - predict(Model_bic, data.test, id = 5))^2)/sqrt(n.test) 


# making a new model based on the 5 precited variables. 
lm_bic<- lm(y~ sex+ bmi+ map+ hdl+ ltg, data =data.train )
summary(lm_bic)
confint(lm_bic)

par(mfrow = c(2, 2))
plot(lm_bic) 
mean((data.test$y - predict(lm_bic, data.test))^2) 
sd((y.test - predict(lm_bic, data.test))^2)/sqrt(n.test) 


# 10 fold cross validation:
k <- 10
set.seed(5410)
folds <- sample(1:k, nrow(data.train), replace = TRUE)
cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))

for (i in 1:k) {
  model3.cv <- regsubsets(y ~ ., data = data.train[folds != i, ], nvmax = 10)
  for (j in 1:10) {
    pred <- predict(model3.cv, data.train[folds == i, ], id = j)
    cv.errors[i, j] = mean((data.train$y[folds == i] - pred)^2)
  }
}

cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
which.min(mean.cv.errors)
mean.cv.errors[8]
plot(mean.cv.errors, type = "b", xlab = "Number of Predictors", ylab = "Mean CV Errors",
     main = "Best Subset Selection (10-fold CV)")
points(8, mean.cv.errors[8], col = "red", cex = 2, pch = 20)

rmse.cv = sqrt(mean.cv.errors)
rmse.cv[8]
plot(rmse.cv, pch = 19, type = "b", xlab = "Number of Predictors", ylab = "RMSE CV",
     main = "Best Subset Selection (10-fold CV)")
points(8, rmse.cv[8], col = "blue", cex = 2, pch = 20)

reg.best <- regsubsets(y ~ ., data = data.train, nvmax = 10)
coef(reg.best, 8)
mean((data.test$y - predict(reg.best, data.test, id = 8))^2) 
sd((data.test$y - predict(reg.best, data.test, id = 8))^2)/sqrt(n.test)

best_model_cv <- lm(y ~ sex + bmi + map + tc + ldl + tch + ltg + glu, data = data.train)
summary(best_model_cv)
coef(best_model_cv)

par(mfrow = c(2, 2))
plot(best_model_cv)

mean((data.test$y - predict(best_model_cv, data.test))^2) 
sd((data.test$y - predict(best_model_cv, data.test))^2)/sqrt(n.test) 

#RIDGE REGRESSION
library(glmnet)
install.packages("Matrix")
install.packages("foreach")
install.packages("caret")
library(Matrix)
library(foreach)
library(caret)

set.seed(5410)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)
plot(cv.out)

largelam = cv.out$lambda.1se
largelam

Model_ridge <- glmnet(x.train, y.train, alpha = 0, lambda = grid)
plot(Model_ridge, xvar = "lambda", label = TRUE)

ridge.pred <- predict(Model_ridge, s=largelam, newx = x.test)
mean((y.test - ridge.pred)^2)
sd((ridge.pred - y.test)^2)/sqrt(n.test) 
coef(Model_ridge, s= largelam)


#LASSO mODEL 
library(glmnet)
par(mfrow = c(1,2))
grid <- 10^seq(10, -2, length = 100)
Model_lasso <- glmnet(x.train, y.train, alpha = 1, lambda = grid)
plot(Model_lasso, xvar = "lambda", label = TRUE)

set.seed(5410)
cv.out = cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out)
largelam = cv.out$lambda.1se
largelam
Model_lasso = glmnet(x.train, y.train, alpha = 1, lambda = largelam)
lasso.pred = predict(Model_lasso, s=largelam, newx = x.test)
mean((lasso.pred - y.test)^2) 
sd((lasso.pred - y.test)^2)/sqrt(n.test)

coef(Model_lasso, s = largelam)


#Model Comparison 
results_df = data.frame(Model = c("1.Full Least Squares Model", "2.Best Subsets Model with BIC", "3.Best Subsets Model with 10-fold CV", "4.Ridge Regression Model with 10-fold CV", "5.Lasso Model with 10-fold CV"), "Test Error" = c( 3095.495, 3073.024,3090.652,3255.365, 3298.973), "Std. Error" = c(368.9687, 382.5969,365.7502,361.3194, 385.5201))
print(results_df, row.names=FALSE)

#best model is the one with 
#MAE
mean((y.test - abs(predict(Model_bic, data.test, id = 5))))
PRED <- predict(Model_bic, data.test, id = 5)

# TO FIND 20TH, 50TH AND 80TH QUANTILES OF THE ERRORS

quantile(y.test - PRED, probs = c(0.20, 0.50, 0.80))

install.packages("Hmisc")
library(e1071)
ku = kurtosis(y.test- PRED)
ku = round(ku,2)
cat(paste0("Kurtosis of the error distribution: ", ku))


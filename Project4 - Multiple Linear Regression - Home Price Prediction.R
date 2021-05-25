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








#Download data
# train data is named as train
#test data is named as test

train = read.csv("C:/Users/anama/Downloads/King County Homes (train).csv")
test = read.csv("C:/Users/anama/Downloads/King County Homes (test).csv")

# lOOKING AT THE COLUMN NAMES
names(train)
names(test)

summary(train)
summary(test)

str(train)
str(test)

#Converting categorical variables to factors:
train$waterfront <- as.factor(train$waterfront)
train$view <- as.factor(train$view)
train$renovated <- as.factor(train$renovated)
train$condition <- as.factor(train$condition)
train$grade <- as.factor(train$grade)


test$waterfront <- as.factor(test$waterfront)
test$view <- as.factor(test$view)
test$renovated <- as.factor(test$renovated)
test$condition <- as.factor(test$condition)
test$grade <- as.factor(test$grade)
summary(train)
summary(test)

library(psych)
describe(train)
str(train)


#Visualizing through histogram and box plot:
hist(train$price, col="blue")
boxplot(train$price, data=train, main="Box plot of Price", col="green")

hist(log(train$price),
     main="Histogram for Air Passengers", 
     xlab="log(Price)", ylab= "Frequency",
     border="blue",
     col="green")



fit1 = lm(price ~ ., data =train)
fit1
summary(fit1)
plot(fit1)

#EDA
boxplot(price~waterfront, data = train, main = "Price by Waterfront")
boxplot(price~view, data = train, main = "Price by view")
boxplot(price~renovated, data = train, main = "Price by renovated")
boxplot(price~condition, data = train, main = "Price by condition")
boxplot(price~grade, data = train, main = "price by grade")



names(train)

describeBy(train[ , c(1:6,11:14,16:20)], group = train$waterfront)
ggpairs(train[ , c(1:6,11:14,16:20)]) # correlation matrix



# Visualize the correlation matrix
library(Hmisc)
r <- rcorr(as.matrix(train))
corrplot(r$r, type='upper',method = "shade", shade.col = NA, p.mat=r$P,
         tl.col="black", tl.srt = 45,number.cex = 1,addCoef.col = 'blue',
         order='hclust',sig.level = 0.05, insig = c("pch"), diag = FALSE,
         col=colorRampPalette(c("red","white","green"))(200))



# Multiple Linear Regression
fit1 = lm(price ~ ., data = train)
fit1
summary(fit1)

# Regression diagnostics,  set up plotting region for 4 plots arranged in 2 rows and 2 columns
par(mfrow = c(2,2))

library(ggfortify)
autoplot(fit1)

par(mfrow=c(1,1))# restore plotting region to contain one plot.


library(olsrr)
ols_plot_cooksd_bar(fit1)  # View cook's distance
ols_plot_dfbetas(fit1) #compute dfbetas (output not shown for the rest) 
ols_plot_dffits(fit1) #compute dffits
ols_plot_resid_stud(fit1) #compute studentized residuals

install.packages("car")
library(car)
avPlots(fit1)




vif(fit1) # not running


# stepwise reduction
fit1.back <- step(fit1, direction = "backward")
anova(fit1.back)
fit1.back$anova




# Stepwise Regression with leaps
library(leaps) 
fit1.full <- regsubsets(price ~ . , data = train,nvmax = 7 )

summary(fit1.full)
reg.summary = summary(fit1.full)
names(reg.summary)



reg.summary
reg.summary$rsq
reg.summary$rss
reg.summary$adjr2
reg.summary$cp
reg.summary$bic



par(mfrow=c(2,2)) # set up a 2 X 2 grid of plots
plot(reg.summary$rss,xlab="Number of Terms",ylab="RSS",type="b")
plot(reg.summary$adjr2,xlab="Number of Terms",ylab="Adjusted R-square",type="b") 
plot(reg.summary$cp,xlab="Number of Terms",ylab="Mallow's Cp",type="b")
plot(reg.summary$bic,xlab="Number of Terms",ylab="BIC",type="b")



# Finding the "optimal" model size using adjusted R2 , Mallow's ck, and BIC
which.max(reg.summary$adjr2)



which.min(reg.summary$cp)



which.min(reg.summary$bic)



par(mfrow=c(2,2))
plot(fit1.full,scale="r2")
plot(fit1.full,scale="adjr2")
plot(fit1.full,scale="Cp")
plot(fit1.full,scale="bic")



# selecting the best fit model
# selected the variables based on stepwise 
bestmodel <- lm(price ~ sqft_living + waterfront  + grade + lat, data = train)
bestmodel
summary(bestmodel)



# crossvalidation
library(caret)
library(leaps)
# set seed 123 
set.seed(123)
train.control<-trainControl(method = "cv", number = 10)



lmcvfit <- train(price ~ ., data = train,
                 method = "lm",
                 trControl = train.control,
                 
                 metric = "RMSE")
summary(lmcvfit)
lmcvfit$results
lmcvfit
lmcvfit$resample
summary(lmcvfit$finalModel)
lmcvfit$bestTune



varImp(lmcvfit)
plot(varImp(lmcvfit))


predictedVal <- predict(bestmodel, newdata = test)

summary(predictedVal)
str(predictedVal)
names(predictedVal)
head(predictedVal)



# saving the predicted price values in the dataframe
modelvalues <- (cbind("ID"=test$ID, "Predicted Price"= predictedVal))




# saving the predicted values in the csv file
write.csv(modelvalues, file = "test_data_anam_Predictedmodel.csv", row.names=FALSE)












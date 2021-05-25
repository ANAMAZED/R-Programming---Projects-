# download data

url ='http://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.zip'
temp = tempfile()
download.file(url,temp, mode="wb")
unzip(zipfile=temp, exdir = './l1')


setwd('l1')
sbase = read.table("spambase.data", sep=",")
cnames = read.table("spambase.names", comment.char="|", header=F)[1]
cnames = gsub("[[:punct:]]", "", as.matrix(cnames))
cnames = c(cnames[c(2:nrow(cnames))],"target")
colnames(sbase) = cnames

# Installing packages
insatll.packages("MASS")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ISLR")
install.packages("GGally")
install.packages("ggplot2")
install.packages("corrplot")

library(MASS)
library(tidyverse)
library(dplyr)
library(ISLR)
library(GGally)
library(ggplot2)
library(corrplot)
library(janitor)

# cleaning duplicate column names

str(sbase)
sbase = clean_names(sbase)
str(sbase)

# Exploring data
str(sbase)
dim(sbase)
names(sbase) 
summary(sbase)
anyNA(sbase)
sbase$target<- as.factor(sbase$target)
str(sbase)
table(sbase$target)
head(sbase)
summary(sbase$target)
summary(sbase$target)


# Propotions of the spam and no spam
prop.table(table(sbase$target))


# histogram
library(ggplot2)
data <- c(1,0)
qplot( sbase$target ) +
  geom_bar() +
  labs(x="Target", y="Count")


#Install package brew
install.packages("brew")
library(plyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(knitr)
library(RColorBrewer)
library(gridBase)
library(caret)
library(randomForest)
library(gbm)
library(corrgram)
library(gplots)
library(RColorBrewer)
library(ROCR)
library(foreign)
CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))
#Visualization
#plotting a barplot for spam vs. not spam and also a pie chart

resTable <- table(sbase$target)
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2) + 0.1)  # increase y-axis margin.
plot <- plot(sbase$target, col = CUSTOM_COLORS_PLOT(2), main = "Spam vs. Not-Spam", 
             ylim = c(0, 4000), ylab = "Number")
text(x = plot, y = resTable + 200, labels = resTable)
percentage <- round(resTable/sum(resTable) * 100)
labels <- paste(row.names(resTable), percentage)  # add percents to labels
labels <- paste(labels, "%", sep = "")  # ad % to labels
pie(resTable, labels = labels, col = CUSTOM_COLORS_PLOT(2), main = "Spam vs. Not-Spam")

#end

# Frequency characteristics of spam and not spam
library(grid)
library(gridBase)

dataset.notspam <- sapply(sbase[which(sbase$target == "0"), 1:54], function(x) ifelse(is.numeric(x), 
                                                                                      round(mean(x), 2), NA))
dataset.spam <- sapply(sbase[which(sbase$target == "1"), 1:54], function(x) ifelse(is.numeric(x), 
                                                                                   round(mean(x), 2), NA))

dataset.notspam.order <- dataset.notspam[order(-dataset.notspam)[1:12]]
dataset.spam.order <- dataset.spam[order(-dataset.spam)[1:12]]

par(mfrow = c(1, 2))
par(mar = c(8, 4, 4, 2) + 0.5)
plot <- barplot(dataset.notspam.order, col = CUSTOM_COLORS_PLOT(12), main = "Not-spam: Average Percentage", 
                names.arg = "" , ylab = "Percentage Relative (%)")
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(dataset.notspam.order), x = unit(plot, "native"), y = unit(-1, 
                                                                           "lines"), just = "right", rot = 50)
popViewport(3)

plot <- barplot(dataset.spam.order, col = CUSTOM_COLORS_PLOT(12), main = "Spam: Average Percentage", 
                names.arg = "", ylab = "Percentage Relative (%)")
vps <- baseViewports()
pushViewport(vps$inner, vps$figure, vps$plot)
grid.text(names(dataset.spam.order), x = unit(plot, "native"), y = unit(-1, 
                                                                        "lines"), just = "right", rot = 50)
popViewport(3)
# the plots show the frequency characteristics of spam ann not spam emails. relative percentage of occurrence of words in a descending order can be seen, e.g. the word "you" is more in spam as compared to not spam

# Correlation plots
sbase$target<- as.numeric(sbase$target)
corrplot(cor(sbase[c(1:10,58)]))
corrplot(cor(sbase[c(10:20,58)]))
corrplot(cor(sbase[c(20:30,58)]))
corrplot(cor(sbase[c(30:40,58)]))
corrplot(cor(sbase[c(40:50,58)]))
corrplot(cor(sbase[c(50:58)]))
#to look for correlation greater than 0.5
vcnCorsData <- abs(sapply(colnames(sbase), detectCor)) #absolute value
summary(vcnCorsData)
vcnCorsData[vcnCorsData>0.5]

#converting back to factor
sbase$target<- as.factor(sbase$target)
# partitioning the dataset into training and test dataset
n <- nrow(sbase)
n
# Number of rows for the training set (70% of the dataset)
n_train <- round(0.70 * n) 
n_train
# Create a vector of indices which is an 70% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)

# Creating training dataset
sbase_train <- sbase[train_indices, ]  

# Exclude the training indices to create the test set
sbase_test <- sbase[-train_indices, ]


# Exploring data by looking at summary, dimensions and struture
str(sbase)
dim(sbase)
names(sbase) 
summary(sbase)
anyNA(sbase)
str(sbase_train)
dim(sbase_train)
dim(sbase_test)


# Model 1 - Tree 
library(rpart)
library(partykit)
library(rcompanion)

tree_sbase <- rpart(target~.,data=sbase_train)

tree_sbase
plot(tree_sbase)
text(tree_sbase)

# The tree gives variables i.e. charfreq_5, wordfreqremove, charfreq_4, capitalrunlengthtotal, wordfreqhp, 
# EDA based on variables selected by using tree 

summary(sbase_train$target)
summary(sbase_train$charfreq_5)
summary(sbase_train$wordfreqremove)
summary(sbase_train$charfreq_4)
summary(sbase_train$capitalrunlengthtotal)
summary(sbase_train$wordfreqhp)
#summary of important variables is seen that were selected using tree method.

#checking if these variables have any mising values
anyNA(sbase_train$charfreq_5)
anyNA(sbase_train$wordfreqremove)
anyNA(sbase_train$charfreq_4)
anyNA(sbase_train$capitalrunlengthtotal)
anyNA(sbase_train$wordfreqhp)


misclass = function(fit,y) {
  temp <- table(fit,y)
  cat("Table of Misclassification\n")
  cat("(row = predicted, col = actual)\n")
  print(temp)
  cat("\n\n")
  numcor <- sum(diag(temp))
  numinc <- length(y) - numcor
  mcr <- numinc/length(y)
  cat(paste("Misclassification Rate = ",format(mcr,digits=3)))
  cat("\n")
}
tree_pred=predict(tree_sbase,newdata=sbase_test, type = "class")
table(tree_pred, sbase_test$target)
tree_pred
misclass(tree_pred,sbase_test$target)


# accuracy of the tree model
confusion.matrix <- table(tree_pred, sbase_test$target)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix)

# Calculate the confusion matrix for the test set
library(caret)
confusionMatrix(data = tree_pred,
                reference = sbase_test$target)



# Model 2 - Linear discriminative Model
lda_sbase <- lda(target ~ ., data = sbase_train, family = binomial(link = "logit"))
lda_pred <- predict(lda_sbase, newdata = sbase_test)
lda_class=lda_pred$class
lda_class
table(lda_class,sbase_test$target)


# accuracy of the tree model
confusion.matrix <- table(lda_pred$class, sbase_test$target)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix)

# confusion Matrix
library(caret)
confusionMatrix(data = lda_pred $class,
                reference = sbase_test$target)


misclass = function(fit,y) {
  temp <- table(fit,y)
  cat("Table of Misclassification\n")
  cat("(row = predicted, col = actual)\n")
  print(temp)
  cat("\n\n")
  numcor <- sum(diag(temp))
  numinc <- length(y) - numcor
  mcr <- numinc/length(y)
  cat(paste("Misclassification Rate = ",format(mcr,digits=3)))
  cat("\n")
}

misclass(lda_pred $class, sbase_test$target)



# Model 3 - Support Vector Machine

library(e1071)

misclass = function (fit,y) 
{
  temp <- table(fit,y)
  cat("Table of Misclassification\n")
  cat("(row = predicted, col = actual)\n")
  print(temp)
  cat("\n\n")
  numcor <- sum(diag(temp))
  numinc <- length(y) - numcor
  mcr <- numinc/length(y)
  cat(paste("Misclassification Rate = ", format(mcr, digits = 3)))
  cat("\n")
}

svm_sbase = svm(target ~ . , data = sbase_train)
misclass(fitted(svm_sbase), sbase_train$target)
summary(svm_sbase)
attributes(svm_sbase)

svm_pred = predict(svm_sbase,newdata = sbase_test)
misclass(svm_pred, sbase_test$target) #  misclassification - 0.0696

#Increasing the cost (C) parameter will increase penalty for misclassifying observations in training data.  This should drive down the misclassification rate on the training data, however this will probably increase test data misclassification due to overfitting.

svm_sbase100 = svm(target ~ ., data = sbase_train, cost = 100)
misclass(fitted(svm_sbase100), sbase_train$target)

svm_pred100 = predict(svm_sbase100, newdata = sbase_test)
misclass(svm_pred100, sbase_test$target)

#not using this model because od very different misclassification rates

#For svm() I use the function tune()to find "optimal" choices for the kernel tuning parameters and the cost (C)

area_tune = tune(svm,target~.,data=sbase_train, 
                 ranges=list(gamma=seq(.05,.5,.05),cost=seq(1,10,1)), 
                 tunecontrol=tune.control(sampling="boot"))

plot(area_tune) #Based on the plot, I'm going to select one of the darkest regions indicating the lowest misclassification rate (gamma=0.15 and cost=6)


svm_sbasetuned = svm(target~.,data=sbase_train,gamma=.15,cost=6)
summary(svm_sbasetuned)
misclass(fitted(svm_sbasetuned),sbase_train$target)

svm_pred_tune = predict(svm_sbasetuned,newdata=sbase_test)
misclass(svm_pred_tune,sbase_test$target)


# Using cross-validation that would allow  to try different combinations of tuning parameters. svm() also has a built-in k-fold cross-validation option as demonstrated below.
svm_sbasecv1 = svm(target~.,data=sbase_train,cross = 5)
summary(svm_sbasecv1)
misclass(fitted(svm_sbasecv1),sbase_train$target)
svm_sbasecv2 = svm(target~.,data=sbase_train,cross = 5, gamma=.15,cost=6)
summary(svm_sbasecv2)
misclass(fitted(svm_sbasecv2),sbase_train$target)
svm_sbasecv3 = svm(target~.,data=sbase_train,cross = 5, gamma=.1,cost=8)
summary(svm_sbasecv3)
misclass(fitted(svm_sbasecv3),sbase_train$target)

svm_pred_cv1 = predict(svm_sbasecv1,newdata=sbase_test)
misclass(svm_pred_cv1,sbase_test$target) # this one is fairly good
summary(svm_pred_cv1)

svm_pred_cv2 = predict(svm_sbasecv2,newdata=sbase_test)
misclass(svm_pred_cv2,sbase_test$target)

svm_pred_cv3 = predict(svm_sbasecv3,newdata=sbase_test)
misclass(svm_pred_cv3,sbase_test$target)


# Calculate the confusion matrix for the test set
library(caret)
confusionMatrix(data = svm_pred,
                reference = sbase_test$target)


# Model 4 - Random Forest.

install.packages("randomForest")
library(randomForest)
set.seed(123)
rf.sbase <- randomForest(target ~ ., data = sbase_train, importance = T)
rf.sbase
rf.pred = predict(rf.sbase,newdata=sbase_test)
misclass(rf.pred,sbase_test$target)
#misclass = 0.0522


#5 mtry = 5
set.seed(123)
summary(rf.sbase)
rf.sbase5 <- randomForest(target ~ ., data = sbase_train, mtry = 5, importance = T)
rf.sbase5
rf.pred5 = predict(rf.sbase5,newdata=sbase_test)
misclass(rf.pred5,sbase_test$target)
#misclass 0.0478 - lowest 


#mtry = 10
set.seed(123)
rf.sbase10 <- randomForest(target ~ ., data = sbase_train, mtry = 10, importance = T)
rf.sbase10
rf.pred10 = predict(rf.sbase10,newdata=sbase_test)
misclass(rf.pred10,sbase_test$target)


set.seed(123)
rf.sbase10 <- randomForest(target ~ ., data = sbase_train, mtry = 100, importance = T)
rf.sbase10
rf.pred10 = predict(rf.sbase10,newdata=sbase_test)
misclass(rf.pred10,sbase_test$target)

#misclass = 0.0522

#The function below displays a bar graph of a predictor importance measure.
rfimp = function(rffit) {barplot(sort(rffit$importance[,1]),horiz=F,
                                 xlab="Mean Decreasing in Accuracy",main="Variable Importance")
}



rfimp(rf.sbase) 



rf.sbase$importance



rf.pred = predict(rf.sbase,newdata=sbase_test)
misclass(rf.pred,sbase_test$target)



# Confusion Matrix
library(caret)
confusionMatrix(data = rf.pred5,
                reference = sbase_test$target)
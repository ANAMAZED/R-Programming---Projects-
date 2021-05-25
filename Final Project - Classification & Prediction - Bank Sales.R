# Installing packages
insatll.packages("MASS")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ISLR")
install.packages("GGally")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("brew")
library(brew)
library(MASS)
library(tidyverse)
library(dplyr)
library(ISLR)
library(GGally)
library(ggplot2)
library(corrplot)
library(janitor)
library(knitr)
library(RColorBrewer)
library(gridBase)
library(caret)
library(randomForest)
library(gbm)
library(corrgram)
library(gplots)
library(plyr)
library(ROCR)
library(foreign)
library(readr)

#Import the dataset.  Change the file path to where you saved your bank.txt file
bank <- read_csv("C:/Users/anama/Downloads/bank.txt", col_types = cols(b_tgt = col_character(), 
                                  int_tgt = col_number(), cnt_tgt = col_double(), 
                                  demog_homeval = col_number(), demog_inc = col_number(), 
                                  rfm1 = col_number(), rfm2 = col_number(), 
                                  rfm3 = col_number(), rfm4 = col_number(), 
                                  demog_genf = col_character(), demog_genm = col_character(), 
                                  dataset = col_character()))

names(bank)
dim(bank)
anyNA(bank)
str(bank)


ans <- bank %>% replace(.=="NULL", NA)
bank <- ans[,colSums(is.na(ans))<nrow(ans)]

#Caret and other packages don't like 0,1 factor levels and will treat them as numeric, lets fix that
bank$b_tgt<-ifelse(bank$b_tgt=="1", "yes", "no")
bank$demog_ho<-ifelse(bank$demog_ho=='1',"yes","no")
bank$demog_genf<-ifelse(bank$demog_genf=="1", "yes","no")
bank$demog_genm<-ifelse(bank$demog_genm=="1", "yes","no")


#Convert factor columns to factors
cols<-c("b_tgt", "cat_input1", "cat_input2","demog_ho","demog_genf", "demog_genm")
bank[cols] <- lapply(bank[cols], factor)
str(bank) #Make sure the data structure looks good

CUSTOM_COLORS_PLOT <- colorRampPalette(brewer.pal(10, "Set3"))
#Visualization of b-tgt
# plotting bar plot 
install.packages("brew")
library(brew)
library(RColorBrewer)
resTable <- table(bank$b_tgt)
par(mfrow = c(1, 2))
par(mar = c(5, 4, 4, 2) + 0.1) # increase y-axis margin.
plot <- plot(bank$b_tgt, col = CUSTOM_COLORS_PLOT(2), main = "New Product - yes vs No",
             ylim = c(0, 1000000), ylab = "Number")
text(x = plot, y = resTable + 200, labels = resTable)
percentage <- round(resTable/sum(resTable) * 100)
labels <- paste(row.names(resTable), percentage) # add percents to labels
labels <- paste(labels, "%", sep = "") # ad % to labels
pie(resTable, labels = labels, col = CUSTOM_COLORS_PLOT(2), main = "New product - yes vs No")

# Propotions of yes vs no
prop.table(table(bank$b_tgt))

hist(log(bank$int_tgt+1), main = "Histogram of New Sales")
hist(bank$cnt_tgt, main = " Histogram of Number of New Products ")
hist(log(bank$cnt_tgt+1), main = " Histogram of Number of New Products ")


# This dataset is very large, thus I would highly encourage you to enable parallel processing. The code below enables you to do that
#Enable parallel CPU processing
install.packages("parallel")
library(doParallel)
detectCores() #Detect how many cores your computer has
cl <- makeCluster(7) #Select your #of cores -1
registerDoParallel(cl)

#To enable parallel processsing, you should set "allowParallel=TRUE", this will allow you to then take advantage of parallel processing
library(caret)
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     allowParallel=TRUE)

# looking for missing values
# Variables found with missing values: int_tgt(848529), cnt_tgt(1), demog_age(266861), rfm3(225786)
sum(is.na(bank))
sum(is.na(bank$b_tgt))
sum(is.na(bank$int_tgt))
sum(is.na(bank$cnt_tgt))
bank$cnt_tgt = replace_na(bank$cnt_tgt, 2)          # Replacing null values with 2
sum(is.na(bank$cnt_tgt))
bank$int_tgt = replace_na(bank$int_tgt,0)            #  Replacing the null values with 0
sum(is.na(bank$int_tgt))
sum(is.na(bank$rfm1))
sum(is.na(bank$rfm2))
sum(is.na(bank$rfm3))
bank$rfm3 = replace_na(bank$rfm3,15.31)             #replacing with mean
sum(is.na(bank$rfm3))
sum(is.na(bank$rfm4))
sum(is.na(bank$rfm5))
sum(is.na(bank$rfm6))
sum(is.na(bank$rfm7))
sum(is.na(bank$rfm8))
sum(is.na(bank$rfm9))
sum(is.na(bank$rfm10))
sum(is.na(bank$rfm11))
sum(is.na(bank$rfm12))
sum(is.na(bank$cat_input1))
sum(is.na(bank$cat_input2))
sum(is.na(bank$demog_homeval))
sum(is.na(bank$demog_inc))
sum(is.na(bank$demog_pr))
sum(is.na(bank$demog_genf))
sum(is.na(bank$demog_genm))
sum(is.na(bank$demog_ho))
sum(is.na(bank$demog_age))
summary(bank$demog_age)
bank$demog_age = replace_na(bank$demog_age,0)
bank$demog_age = cut(bank$demog_age, breaks = c(-2,18,40,89))  #grouping the age 
summary(bank$demog_age)
#bank$demog_age = replace_na(bank$demog_age, 60)          #replacing with median
sum(is.na(bank$demog_age))



# Detecting outliers 
boxplot(bank$rfm1,bank$rfm2,bank$rfm3,bank$rfm4, main = " Boxplot of rfm1, rfm2, rfm3, rfm4")
boxplot(bank$rfm5,bank$rfm6,bank$rfm7,bank$rfm8, main = " Boxplot of rfm5, rfm6, rfm7, rfm8,")
boxplot(bank$rfm9,bank$rfm10,bank$rfm11,bank$rfm12, main = " Boxplot of rfm9, rfm10, rfm11, rfm12,")
boxplot(bank$demog_homeval, main = " Boxplot of homevalue")
boxplot(bank$demog_inc, main = "Boxplot of income")
boxplot(bank$demog_pr, main = " Boxplot of % retired in Area")
boxplot(bank$demog_age, main = "Boxplot of Customer Age")
boxplot(bank$int_tgt, main ="Boxplot of New Sales")
boxplot(bank$cnt_tgt, main= "Boxplot of count of New Products")

#to check outliers
Outliervalues = boxplot.stats(bank$int_tgt)$out 
Outliervalues


Outliervalues= boxplot.stats(bank$cnt_tgt)$out
Outliervalues

Outliervalues = boxplot.stats(bank$rfm3)$out
Outliervalues

#Lets split our dataset into the 3 subsets (training, validation, testing)
set.seed(123)
bank_train<- bank[ which(bank$dataset=='1'),]
bank_train
bank_validation<-bank[ which(bank$dataset=='2'),]
bank_validation
bank_test<-bank[ which(bank$dataset=='3'),]
bank_test


# handling the outliers by normalizing variables using center and scale technique
#preProcValues <- preProcess(bank, method = c("center", "scale"))

#bank_train <- predict(preProcValues, bank_train)
#names(bank_train)
#bank_test <- predict(preProcValues, bank_test)
#bank_validation <- predict(preProcValues, bank_validation)


# checking the distribution of data



hist(bank$rfm1, main = "Histogram of rfm1")
hist(log(bank$rfm1+1), main = "Histogram of log rfm1")     # log transformation



hist(bank$rfm2, main = "Histogram of rfm2")
hist(log(bank$rfm2+1), main = "Histogram of log rfm2")       # log transformation



hist(bank$rfm3, main = "Histogram of rfm3")
hist(log(bank$rfm3+1), main = "Histogram of log rfm3")       # log transformation



hist(bank$rfm4, main = "Histogram of rfm4")
hist(log(bank$rfm4+1), main = "Histogram of log rfm4")        # log transformation



hist(bank$rfm5, main = "Histogram of rfm5")
hist(log(bank$rfm5), main = "Histogram of log rfm5")         # log tranformation didn't work
hist(scale(bank$rfm5), main = "Histogram of scale rfm5")     # Scale transformation



hist(bank$rfm6, main = "Histogram of rfm6")               #  rfm6 - log transformation didn't work, use scale 
hist(log(bank$rfm6), main = "Histogram of log rfm6")
hist(scale(bank$rfm6), main = "Histogram of scale rfm6")

hist(bank$rfm7, main = "Histogram of rfm7")               #  rfm7 - log transformation didn't work, use scale 
hist(log(bank$rfm7), main = "Histogram of log rfm7")
hist(scale(bank$rfm7), main = "Histogram of scale rfm7") 



hist(bank$rfm8, main = "Histogram of rfm8")               
hist(log(bank$rfm8), main = "Histogram of log rfm8")          #  rfm8 - log transformation didn't work, use scale 
hist(scale(bank$rfm8), main = "Histogram of scale rfm8")     # scale transformation



hist(bank$rfm9, main = "Histogram of rfm9")               
hist(log(bank$rfm9), main = "Histogram of log rfm9")          #  rfm9 - log transformation didn't work, use scale 
hist(scale(bank$rfm9), main = "Histogram of scale rfm9") 



hist(bank$rfm10, main = "Histogram of rfm10")               
hist(log(bank$rfm10+1), main = "Histogram of log rfm10")          #  rfm10 - log transformation used 



hist(bank$rfm11, main = "Histogram of rfm11")            
hist(bank$rfm12, main = "Histogram of rfm12")               #  rfm12 - transformation not needed



hist(bank$demog_homeval, main= "Histogram of Homevalue")
hist(log(bank$demog_homeval+1), main= "Histogram of log Homevalue")   # log transformation used



hist(bank$demog_age, main= "Histogram of Customer age")        # transformation not needed



hist(bank$demog_inc, main = "Histogram of Income")
hist(log(bank$demog_inc+1), main= "Histogram of log Income")      # log transformation used



hist(bank$demog_pr, main = "Histogram of % of Retired in Area")   # log transformation not needed

# checking Outliers
boxplot(bank$demog_inc ~ bank$b_tgt, main = " Income vs New product")
boxplot(bank$rfm1~ bank$b_tgt, main = " Avg. Sales past 3 years(rfm1) vs New product")
boxplot(bank$rfm2~ bank$b_tgt, main = " Avg. Sales Lifetime (rfm2) vs New product")
boxplot(bank$rfm3~ bank$b_tgt, main = "Average Sales Past Three Years Dir Promo Resp (rfm3)vs New product")
boxplot(bank$rfm4~ bank$b_tgt, main = " Last Product Purchase Amount rfm4) vs New product")
boxplot(bank$rfm5~ bank$b_tgt, main = "Count Purchased Past 3 Years (rfm5) vs New product")
boxplot(bank$rfm6~ bank$b_tgt, main = "Count Purchased Lifetime(rfm6) vs New product")
boxplot(bank$rfm7~ bank$b_tgt, main = "Count Purchased Past 3 Years Dir Promo Resp (rfm7) vs New product")
boxplot(bank$rfm8~ bank$b_tgt, main = "Count Purchased Lifetime Dir Promo Resp(rfm8) vs New product")
boxplot(bank$rfm9~ bank$b_tgt, main = "Months Since Last Purchase (rfm9) vs New product")
boxplot(bank$rfm10~ bank$b_tgt, main = "Count Total Promos Past Year (rfm10) vs New product")
boxplot(bank$rfm11~ bank$b_tgt, main = "Count Direct Promos Past Year (rfm11) vs New product")
boxplot(bank$rfm12~ bank$b_tgt, main = "Customer Tenure (rfm12) vs New product")
boxplot(bank$demog_age ~ bank$b_tgt, main = " age vs New product")
boxplot(bank$demog_homeval ~ bank$b_tgt, main = " Home value vs New product")
boxplot(bank$demog_pr ~ bank$b_tgt, main = " percentage retired in area vs New product")

#Decision Tree model
Sys.time()
dt <- train(b_tgt ~cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval+demog_inc+demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+ rfm10+rfm11+rfm12 + demog_genf,
            data=bank_train,
            method = "rpart",
            trControl = ctrl,
            metric="ROC",
            na.action = "na.omit")



Sys.time()

summary(dt)

#It ran in ~ 4 minutes on my machine
# variable importance = rfm1, rfm2, rfm3, rfm4, rfm5, rfm6, rfm7, rfm8, and demog_homeval

#............................................................................................................................................................................

# Tranformation of variables:
rfm1 <- log(bank$rfm1+1)
rfm2 <- log(bank$rfm2+1)
rfm3 <- log(bank$rfm3+1)
rfm4 <- log(bank$rfm4+1)
rfm5 <- scale(bank$rfm5)
rfm6 <- scale(bank$rfm6)
rfm7 <- scale(bank$rfm7)
rfm8 <- scale(bank$rfm8)
rfm9 <- scale(bank$rfm9)
rfm10 <- log(bank$rfm10+1)
demog_homeval <- log(bank$demog_homeval+1)
demog_inc <- log(bank$demog_inc+1)
#rfm11, rfm12, demog_ade, demog_ho, demog_pr, cat_input1, cat_input2, demog_genf, demog_genm, 
bank_train
bank_test

#............................................................................................................................................................................
# Linear discriminative Model
#Now begin to fit your model suite. Fit the following models:
# A linear discriminant anlaysis(LDA/QDA) 

lda.bank <- lda(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train,family = binomial(link = "logit"))

lda.pred_train <- predict(lda.bank, newdata = bank_train)
lda.class=lda.pred_train$class
lda.class
table(lda.class,bank_train$b_tgt)

# accuracy of the LDA model
confusion.matrix <- table(lda.pred_train$class, bank_train$b_tgt)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix)



# confusion Matrix
library(caret)
confusionMatrix(data = lda.pred_train $class,
                reference = bank_train$b_tgt)

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



misclass(lda.pred_train $class, bank_train$b_tgt)

# predicting on  bank_validation
lda.pred_valid <- predict(lda.bank, newdata = bank_validation)
lda.class=lda.pred_valid$class
lda.class
table(lda.class,bank_validation$b_tgt)



# accuracy of the lda.pred_valid model
confusion.matrix <- table(lda.pred_valid$class, bank_validation$b_tgt)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix)



# confusion Matrix
library(caret)
confusionMatrix(data = lda.pred_train $class,
                reference = bank_train$b_tgt)

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



misclass(lda.pred_valid $class, bank_validation$b_tgt)
#................................................................................................................................................................
# tree classification
library(rpart)
library(partykit)
library(rcompanion)

# Tree model
tree.bank <- rpart(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train)



tree.bank
plot(tree.bank)
text(tree.bank)
# important variables = rfm5, rfm2, rmf9, demog_homeval



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

tree.pred_train=predict(tree.bank,newdata=bank_train, type = "class")
table(tree.pred_train, bank_train$b_tgt)



misclass(tree.pred_train,bank_train$b_tgt)




# accuracy of the tree model
confusion.matrix <- table(tree.pred_train, bank_train$b_tgt)
confusion.matrix
sum(diag(confusion.matrix)) / sum(confusion.matrix)



# Calculate the confusion matrix for the traint set
library(caret)
confusionMatrix(data = tree.pred_train,
                reference = bank_train$b_tgt)

# validating on bank_validation



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
tree.pred_valid=predict(tree.bank,newdata=bank_validation, type = "class")
table(tree.pred_valid, bank_validation$b_tgt)



misclass(tree.pred_valid,bank_validation$b_tgt)

#..........................................................................................................................................................................


# Logistic Regression
library(ISLR)
glm.fit <- glm(b_tgt ~ rfm1 + rfm2 + rfm3 + rfm4 + rfm5 + rfm6 + rfm7 + rfm8 + demog_homeval, data = bank_train, family = "binomial")
summary(glm.fit)
coef(glm.fit) # get the coefficients of the fitted model
summary(glm.fit)$coef[,4] # get the p-values for the coefficients

prop.table(table(bank_train$b_tgt))

# Predictions on the training set
predictTrain = predict(glm.fit, data = bank_train, type = "response")
# Confusion matrix on training data
table(bank_train$b_tgt, predictTrain >= 0.5)


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
#Predictions on the valid set
predictvalid = predict(glm.fit, newdata = bank_validation, type = "response")
# Confusion matrix on test set
table(bank_validation$b_tgt, predictvalid >= 0.5)
misclass(predictvalid,bank_validation$b_tgt)

# Calculate the confusion matrix for the validation set

library(caret)
confusionMatrix(data = predictvalid,
                reference = bank_validation$b_tgt)
#........................................................................................................................................................................

#NAIVE BAYES
library(e1071)
nb.fit=naiveBayes(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data= bank_train)


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

#TRAIN DATSET ..............................................................................................
ypred=predict(nb.fit,newdata = bank_train)
misclass(ypred,bank_train$b_tgt)


#VALIDATION DATASET
ypredv=predict(nb.fit,newdata = bank_validation)
misclass(ypredv,bank_validation$b_tgt)


library(caret)
confusionMatrix(data = ypred,
                reference = bank_train$b_tgt)


library(caret)
confusionMatrix(data = ypredv,
                reference = bank_validation$b_tgt)

#TEST DATASET AS THIS MODEL IS THE BEST .................................................................

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

#TEST DATSET
ypredt=predict(nb.fit,newdata = bank_test)
misclass(ypredt,bank_test$b_tgt)

library(caret)
confusionMatrix(data = ypredt,
                reference = bank_test$b_tgt)
#.................................................................................................................................................................................

# Question No2 - Predictive Modeling - variable: int_tgt
#Transforming the response variable
int_tgt <- log(bank$int_tgt+1)

#Using tree model to identify important variables 
library(rpart)
library(partykit)
library(rcompanion)

# Tree model
tree.bank <- rpart(int_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train)

tree.bank
plot(tree.bank)
text(tree.bank)

#Important Variables: 

#Linear Model for varibales:rfm5, rfm1, rfm9, demog_homeval, rfm10

#Linear Model - Training data 
Model_lm = lm(int_tgt ~ rfm1 + rfm5 + rfm9 + rfm10 +  demog_homeval, data = bank_train)
summary(Model_lm)
coef(Model_lm)
par(mfrow = c(2,2))
plot(Model_lm)

#instead of defining y.test, i used the values of int_tgt[bank_vaidation] in the formula for error and for n.test, i used dim(bank_validation)[1]
pred = predict(Model_lm, newdata = bank_validation)
mse <- mean((bank_validation$int_tgt - pred)^2) 
mse
sqrt(mse)
sqrt(mse)
R2 = R2(pred, bank_validation$int_tgt)
R2


# 
# BEST SUBSET SELECTION - USING BIC 
library(leaps)
Model_bic <- regsubsets(int_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train, nvmax = 7)
summary(Model_bic)
par(mfrow = c(1, 2))
plot(Model_bic, scale = "bic", main = "Predictor Variables vs. BIC")
summary(Model_bic)
summary(Model_bic)$bic
plot(summary(Model_bic)$bic, xlab = "Number of Predictors", ylab = "BIC", type = "l",
     main = "Best Subset Selection Using BIC")
which.min(summary(Model_bic)$bic)
points(8, summary(Model_bic)$bic[8], col = "green", cex = 2, pch = 20)
coef(Model_bic, 8)

predict.regsubsets <- function(object, newdata, id,...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  cof <- coef(object, id = id)
  xvars <- names(cof)
  mat[, xvars]%*%cof
}

pred.bic = predict(Model_bic, newdata = bank_validation, id = 8)
mean((bank_validation$int_tgt - pred.bic)^2)

# making a new model based on the 8 precited variables. 
lm.bic <- lm(int_tgt ~ rfm1 + rfm3 + rfm4 + rfm5 + rfm9 +rfm11 + rfm12 + demog_homeval, data = bank_train)
summary(lm.bic)
confint(lm.bic)

par(mfrow = c(2, 2))
plot(lm.bic) 
pred.bic8 <- predict(lm.bic, newdata = bank_validation)
mean((bank_validation$int_tgt - pred.bic8)^2) 
bank_train
sqrt(mse)
R2 = R2(pred.bic, bank_validation$int_tgt)
R2

# GAM model with variables from bic model - 8 variables .............................................................................
library(mgcv)
install.packages("gam")
library(gam)
gam_bank <- gam(int_tgt ~ s(rfm1)  +s(rfm3) + s(rfm4) + s(rfm5) + s(rfm6) + s(rfm7) + s(rfm9)  + s(rfm11) + s(rfm12) + s(demog_homeval)   ,data=bank_train)
summary(gam_bank)
gam_pred <- predict(gam_bank, newdata = bank_validation)
mean((bank_validation$int_tgt - gam_pred )^2)

# Test set 
gam_pred <- predict(gam_bank, newdata = bank_test)
mse = mean((bank_test$int_tgt - gam_pred)^2)
R2 = R2(gam_pred, bank_test$int_tgt)
R2
mse
sqrt(mse)


#Naive Bayes ............not using ..........................................................................................
library(e1071)
NB.fit=naiveBayes(int_tgt ~ rfm1  +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 +rfm9 + rfm11 + rfm12 + demog_homeval  ,data= bank_train)

# validation dataset
ypred.nb=predict(NB.fit,newdata = bank_validation)
ypred.nb

mean(bank_validation$int_tgt - ypred.nb)^2
R2 = R2(ypred.nb, bank_validation$int_tgt)
R2

# poisson less 
pr_fit <- glm(int_tgt ~cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval   +demog_inc +demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+rfm10  +rfm11+rfm12 + demog_genf + demog_genm, data=bank_train)
pr_fit

pr_pred = predict(pr_fit, newdata = bank_validation)

mean((bank_validation$int_tgt - pr_pred)^2)
summary(pr_pred)
R2 <- R2(pr_pred, bank_validation$int_tgt)
R2

#Model 3 regsubset with cross validation
k <- 10
set.seed(123)
folds <- sample(1:k, nrow(bank_train), replace = TRUE)
cv.errors <- matrix(NA, k, 10, dimnames = list(NULL, paste(1:10)))




for (i in 1:k) {
  int_tgt_cv <- regsubsets(int_tgt ~ cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval   +demog_inc +demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+        rfm10  +rfm11+rfm12 + demog_genf + demog_genm, data = bank_train[folds != i, ], nvmax = 10)
  for (j in 1:10) {
    pred <- predict(int_tgt_cv, bank_train[folds == i, ], id = j)
    cv.errors[i, j] = mean((bank_train$int_tgt[folds == i] - pred)^2)
  }
}



cv.errors



mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
mincv = which.min(mean.cv.errors)
mean.cv.errors[mincv]



par(mfrow = c(1,2))
plot(mean.cv.errors, type = "b", xlab = "Number of Predictors", ylab = "Mean CV Errors",
     main = "Best Subset Selection (10-fold CV)")
points(mincv, mean.cv.errors[mincv], col = "brown", cex = 2, pch = 20)



rmse.cv = sqrt(mean.cv.errors)
rmse.cv[mincv]



plot(rmse.cv, pch = 19, type = "b", xlab = "Number of Predictors", ylab = "RMSE CV",
     main = "Best Subset Selection (10-fold CV)")
points(mincv, rmse.cv[mincv], col = "blue", cex = 2, pch = 20)



reg.best <- regsubsets(int_tgt ~ cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval   +demog_inc +demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+        rfm10  +rfm11+rfm12 + demog_genf + demog_genm, data = bank_train, nvmax = 10)
coef(reg.best, mincv)



mean((bank_validation$int_tgt - predict(reg.best, bank_validation, id = mincv))^2) 



sqrt(mean((bank_validation$int_tgt - predict(reg.best, bank_validation, id = mincv))^2))
#Fitting linear model with best subset from CV
lm.cv.best <- lm(int_tgt ~ cat_input2 +demog_homeval+rfm2+rfm3+ rfm5+ rfm9+       rfm12, data = bank_train)
summary(lm.cv.best) 



# Plot the model diagnostics
par(mfrow = c(2, 2))
plot(lm.cv.best)


# ............................................................................................................................................................#

#Poisson 

pr.fit <- glm(cnt_tgt ~cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval +demog_inc +demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+ rfm10 +rfm11+rfm12 + demog_genf + demog_genm, family = "poisson",  data=bank_train)



pr.fit



pr.pred = predict(pr.fit, newdata = bank_validation)
summary(pr.pred)
mse = mean((bank_validation$int_tgt - pr.pred)^2)
mse
sqrt(mse)
R2 = R2(pr.pred, bank_validation$cnt_tgt)
R2

pr.pred = predict(pr.fit, newdata = bank_test)
summary(pr.pred)
mse = mean((bank_test$int_tgt - pr.pred)^2)
mse
sqrt(mse)
R2 = R2(pr.pred, bank_test$cnt_tgt)
R2


#POISSON FOR PREDICTING CNT_TGT

set.seed(123)

#Poisson with full train set 

poisson_cnt_tgt = glm(cnt_tgt ~ cat_input1+cat_input2 +demog_age+demog_ho +demog_homeval +demog_inc +demog_pr+rfm1+rfm2+rfm3+ rfm4+ rfm5+ rfm6+rfm7 + rfm8+ rfm9+ rfm10 +rfm11+rfm12 + demog_genf + demog_genm, family="poisson", data=bank_train)

poisson_cnt_tgt
summary(poisson_cnt_tgt)

library(sandwich)

cov.poisson_cnt_tgt = vcovHC(poisson_cnt_tgt, type="HC0")
std.err = sqrt(diag(cov.poisson_cnt_tgt))
r.est = cbind(Estimate= coef(poisson_cnt_tgt), "Robust SE" = std.err,
              "Pr(>|z|)" = 2 * pnorm(abs(coef(poisson_cnt_tgt)/std.err), lower.tail=FALSE),
              LL = coef(poisson_cnt_tgt) - 1.96 * std.err,
              UL = coef(poisson_cnt_tgt) + 1.96 * std.err)

r.est

with(poisson_cnt_tgt, cbind(res.deviance = deviance, df = df.residual,
                            p = pchisq(deviance, df.residual, lower.tail=FALSE)))


# Predict on validation data
pred_validate_poisson_cnt_tgt = predict(poisson_cnt_tgt, newdata = bank_validation, type="response")
pred_validate_poisson_cnt_tgt


# Model performance metrics
data.frame(
  RMSE = caret::RMSE(pred_validate_poisson_cnt_tgt, bank_validation$cnt_tgt),
  Rsquare = caret::R2(pred_validate_poisson_cnt_tgt, bank_validation$cnt_tgt)
)


# Predict on training data
pred_test_poisson_cnt_tgt = predict(poisson_cnt_tgt, newdata = bank_test, type="response")

# Model performance metrics
data.frame(
  RMSE = caret::RMSE(pred_test_poisson_cnt_tgt, bank_test$cnt_tgt),
  Rsquare = caret::R2(pred_test_poisson_cnt_tgt, bank_test$cnt_tgt)
)



















# C. Support Vector Machine



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



svm.bank = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train)
misclass(fitted(svm.sbase), bank_train$b_tgt)
summary(svm.bank)
attributes(svm.bank)



# Prediction on bank_train
svm.pred_train = predict(svm.bank,newdata = bank_train)
misclass(svm.pred_train, bank_train$b_tgt) 

# prediction on bank_validation
svm.pred_valid = predict(svm.bank,newdata = bank_validation)
misclass(svm.pred_valid, bank_validation$b_tgt)



#Increasing the cost (C) parameter will increase penalty for misclassifying observations in training data.  This should drive down the misclassification rate on the training data, however this will probably increase test data misclassification due to overfitting.



svm.bank100 = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train, cost = 100)



# bank_train dataset
misclass(fitted(svm.bank100), bank_train$b_tgt)



svm.pred100 = predict(svm.bank100, newdata = bank_train)
misclass(svm.pred100, bank_train$b_tgt)



# bank_validation data
misclass(fitted(svm.bank100), bank_validation$b_tgt)



svm.pred100 = predict(svm.bank100, newdata = bank_validation)
misclass(svm.pred100, bank_validation$b_tgt)

#For svm() I use the function tune()to find "optimal" choices for the kernel tuning parameters and the cost (C)



area.tune = tune(svm,b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train,data=bank_train, 
                 ranges=list(gamma=seq(.05,.5,.05),cost=seq(1,10,1)), 
                 tunecontrol=tune.control(sampling="boot"))



plot(area.tune) #Based on the plot, I'm going to select one of the darkest regions indicating the lowest misclassification rate (gamma=0.15 and cost=6)




svm.banktuned = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train,gamma=.15,cost=6)
summary(svm.banktuned)



# on training data
misclass(fitted(svm.banktuned),bank_train$target)
svm.pred.tune = predict(svm.banktuned,newdata=bank_train)
misclass(svm.pred.tune,bank_train$b_tgt)



# on validation data
misclass(fitted(svm.banktuned),bank_validation$target)
svm.pred.tune = predict(svm.banktuned,newdata=bank_validation)
misclass(svm.pred.tune,bank_validation$b_tgt)

# Using cross-validation that would allow  to try different combinations of tuning parameters. svm() also has a built-in k-fold cross-validation option as demonstrated below.
svm.bankcv1 = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train,cross = 5)
summary(svm.bankcv1)
misclass(fitted(svm.bankcv1),bank_train$b_tgt)



svm.bankcv2 = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train,cross = 5, gamma=.15,cost=6)   
summary(svm.bankcv2)
misclass(fitted(svm.bankcv2),bank_train$b_tgt)



svm.bankcv3 = svm(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 ,data=bank_train,cross = 5, gamma=.1,cost=8)
summary(svm.bankcv3)
misclass(fitted(svm.bankcv3),bank_train$b_tgt)



svm.pred.cv1 = predict(svm.bankcv1,newdata=bank_validation)
misclass(svm.pred.cv1,bank_validation$b_tgt) # this one is fairly good
summary(svm.pred.cv1)



svm.pred.cv2 = predict(svm.bankcv2,newdata=bank_validation)
misclass(svm.pred.cv2,bank_validation$b_tgt)



svm.pred.cv3 = predict(svm.bankcv3,newdata=bank_validation)
misclass(svm.pred.cv3,bank_validation$b_tgt)


# Calculate the confusion matrix for the validation set
library(caret)
confusionMatrix(data = svm.pred,
                reference = bank_validation$b_tgt)


confusionMatrix(data = svm.pred.cv1,
                reference = bank_validation$b_tgt)

# Model 4 - Random Forest.

sum(is.na(bank_train))

install.packages("randomForest")
library(randomForest)
set.seed(123)
rf.bank <- randomForest(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train, importance = T )



rf.bank

#TRAIN DATASET:
rf.pred = predict(rf.bank,newdata=bank_train)
misclass(rf.pred,bank_train$b_tgt)



#VALIDATION DATASET: 
rf.predv = predict(rf.bank,newdata=bank_validation)
misclass(rf.predv,bank_validation$b_tgt)

#5 mtry = 5
set.seed(123)
summary(rf.bank)
rf.bank5 <- randomForest(b_tgt ~ . rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train , data = bank_train, mtry = 5, importance = T)
rf.sbase5




#TRAIN DATASET:
rf.pred5 = predict(rf.bank5,newdata=bank_train)
misclass(rf.pred5,bank_train$b_tgt)




#VALIDATION DATASET: 
rf.pred5v = predict(rf.bank5,newdata=bank_validation)
misclass(rf.pred5v,bank_validation$b_tgt)

#mtry = 10
set.seed(123)
rf.bank10 <- randomForest(b_tgt ~ . rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train , mtry = 10, importance = T)
rf.bank10




#TRAIN DATASET:
rf.pred10 = predict(rf.bank10,newdata=bank_train)
misclass(rf.pred10, bank_train$b_tgt)




#VALIDATION DATASET: 
rf.pred10v = predict(rf.bank10,newdata=bank_validation)
misclass(rf.pred10v, bank_validation$b_tgt)

# mtry=100
set.seed(123)
rf.bank100 <- randomForest(b_tgt ~ rfm1 + rfm2 +rfm3 +rfm4 + rfm5 + rfm6 + rfm7 + rfm8 +rfm9 + rfm10 + rfm11 + rfm12 + demog_homeval +demog_inc + demog_age + demog_ho + demog_pr + demog_genf + demog_genm + cat_input1 + cat_input2 , data = bank_train , mtry = 100, importance = T)
rf.bank100




#TRAIN DATASET
rf.pred100 = predict(rf.bank100,newdata=bank_train)
misclass(rf.pred100,bank_train$b_tgt)




#Validation Dataset
rf.pred100v = predict(rf.bank100,newdata=bank_validation)
misclass(rf.pred100v,bank_validation$b_tgt)




#The function below displays a bar graph of a predictor importance measure.
rfimp = function(rffit) {barplot(sort(rffit$importance[,1]),horiz=F,
                                 xlab="Mean Decreasing in Accuracy",main="Variable Importance")
}


rfimp(rf.bank) 



rf.bank$importance


# rf.pred = predict(rf.bank,newdata=bank_test)
# misclass(rf.pred,bank_test$b_tgt

# Confusion Matrix
#library(caret)
#confusionMatrix(data = rf.pred5,
#reference = bank_test$b_tgt)


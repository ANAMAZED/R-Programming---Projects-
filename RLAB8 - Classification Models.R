library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)


#predicting with lag1, 2 and 3 only.
glm.fits=glm(Direction~Lag1+Lag2+Lag3,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
# Based on the p-values, none of the predictors are significant.


#Test sensitivity
glm.fits=glm(Direction~Lag1+Lag2+Lag3,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(551+114)/1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
#Sensitivity: tp/tp+fn = 110/(110+31) = 78.01%
#considering up as positive case 1, and down as negative 0.

#LDA 
library(MASS)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
lda.fit=lda(Direction~Lag1+Lag2+Lag3,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

# Specificity: tn/tn+fp = 38/(38+73)=38/111= 34.23%

#Q4
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)
min(lda.pred$posterior)
# 45.77 ~ 46


#Q5
qda.fit=qda(Direction~Lag1+Lag2+Lag3,data=Smarket,subset=train)
qda.fit
qda.class=predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class==Direction.2005)
#Precision = tp/tp+fp = 120/120+85 = 58.54% ~59

#Q6
library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

set.seed(123)
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)

#WHEN SEED IS SET AT 1, CONFUSION MATRIX IS SAME AS BOOK FOR BOTH K=1 AND K=3, BUT WHEN SEED IS SET TO 123 THE CONFUSION MATRIX IS NOT SAME AS TO THE ONE IN BOOK THEREFORE I HAVE SELECTED FALSE. 


#q7

dim(Caravan)
attach(Caravan)
summary(Purchase)
348/5822
standardized.X=scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])


lda.fit=lda(Purchase~.,data=Caravan,subset=-test)
lda.probs=predict(lda.fit,Caravan[test,])$posterior[,2] 
lda.pred=rep("No",1000) 
lda.pred[lda.probs>.25]="Yes"
table(lda.pred,test.Y)

#Actual purchasers predicted to purchase = 13
#Actual purchasers not predicted to purchase=46
#Actual non-purchasers predicted to purchase=27
#Actual non-purchasers not predicted to purchase=914
#Correctly predicted Purchases=927
#Incorrectly predicted purchases=73


#Q8 
install.packages("e1071")
library(e1071)
test=1:1000
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
nb_default <- naiveBayes(Purchase~.,data=Caravan , subset=-test)
default_pred <- predict(nb_default, test, type="class")
table(default_pred,test$Purchase,dnn=c("Prediction", "Actual"))

      
      


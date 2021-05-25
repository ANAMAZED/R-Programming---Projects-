# Question No. 1 
# Load library - ISLR
library(ISLR)
#Checking if dataset exists:
?Auto

#sETTING SEED:
set.seed(123)
train = sample( nrow(Auto),floor(nrow(Auto)/2) )
#train=sample(392,196)

#Linear Regression:
#model 1
lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train)
predict_1= predict(lm.fit, Auto[-train,])
MSE1= mean((Auto$mpg[-train] - predict_1)^2)


#model 2
lm.fit2 = lm(mpg ~ horsepower + I(horsepower^2), data = Auto, subset = train)
predict_2 = predict(lm.fit2, Auto[-train,])
MSE2 = mean((Auto$mpg[-train]-predict_2)^2)

#model 3
lm.fit3 = lm(mpg ~ poly(horsepower,3), data = Auto, subset = train)
predict_3 = predict(lm.fit3, Auto[-train,])
MSE3 = mean((Auto$mpg[-train]-predict_3)^2)

MSE1
MSE2
MSE3


#MSE1 = 21.25, MSE2 = 16.48, MSE3 = 16.58
#Using R code from lab tutorial gives same values:
set.seed(123)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)


#Question No. 2:
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
cv.error=rep(0,6)
for (i in 1:6){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
# I have selected the 6th element which is 18.97864 ~ 18.98 MSE value for the sixth-order polynomial

#Question No.4:
set.seed(5900)
cv.error.5=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.5[i]=cv.glm(Auto,glm.fit,K=5)$delta[1]
}
cv.error.5
cv.error.5[1]
cv.error.5[2]
# First element which is the MSE FOR order one of linear regression model = 24.07906 ~ 24.09
# Second element which is the MSE for quadratic order two of regression model = 19.17175 ~ 19.17

#Question No.5:
alpha.fn=function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio,1:100)
set.seed(2020)
alpha.fn(Portfolio,sample(100,100,replace=T))
boot(Portfolio,alpha.fn,R=1000)

# Bootstrap estimate for standard error of alpha - hat is 0.09084931 ~ 0.091


#Question No.6: We will use the Bootstrap Statistics

boot.fn=function(data,index)
  return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(123)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(123)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef

# the bootstrap estimate for standard error of intercept is 2.2148067000 ~2.215, horsepower = 0.0356258232 ~ 0.0356, horsepower^2 = 0.0001294412 ~ 0.0001

# Question No.7:
set.seed(1)
n = 100
x = rnorm(n)
y = x - 2 * (x^2) + rnorm(n)
plot(y~x, data = Auto,)
#  we can see an upside down bowl which represents a negative quadratic term 
  


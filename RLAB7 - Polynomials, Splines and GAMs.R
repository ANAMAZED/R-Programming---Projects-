#Load dataset
library(ISLR)
attach(Wage)

fit=lm(wage~poly(age,4),data=Wage)
coef(summary(fit))
fit2=lm(wage~poly(age,4,raw=T),data=Wage)
coef(summary(fit2))
fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage)
coef(fit2a)
fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage)
agelims=range(age)
age.grid=seq(from=agelims[1],to=agelims[2])
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit)
par(mfrow=c(1,2),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Degree-4 Polynomial",outer=T)
lines(age.grid,preds$fit,lwd=2,col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE)
max(abs(preds$fit-preds2$fit))


#Question No. 1:
fit.1=lm(wage~education+age,data=Wage)
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3)
# The p-value for model 1 vs. model 2 is essentially zero, and the p-value for model 2 vs. model 3 is approximately 3%. (0.0341)

# Question No.2
coef(summary(fit.3))
# I looked at the t-value for poly(age,3)3 which is 2.119808 ~ 2.12

#Question No.3:
table(cut(age,c(0,25,40,60,80)))
fit=lm(wage~cut(age, c(0,25,40,60,80)),data=Wage)
coef(summary(fit))

# (0,25] = $76K (76.28175)
# (26,40] = $110K (76.28 + 33.53 = 109.81)
# (41,60] = $118 (76.28+42.15 = 118.43)
# (61, 80] = $113 (76.28+ 36.48 = 112.76)


#Question No.4:
library(splines)
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
predict(fit, data.frame(age = 80)) # 77.09986
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
predict(fit2, data.frame(age = 80)) # 95.88467
fit3 <- smooth.spline(age, wage, cv = FALSE)
fit3$y[fit3$x==80] # 88.03068
fit4 <- loess(wage ~ age, span = .5, data = Wage)
predict(fit4, data.frame(age = 80)) # 79.06454

#Question No.5:
install.packages("gam")
library(gam)
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.Gam(gam1, se=TRUE, col="red")
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m2)
coef(gam.m2)
# 62.6007203 ~ 62.6

# Question No.6:
predict(gam.m2,data.frame(year=2008,age=51,education="3. Some College"))
# 117.0654 ~ 117

# Question No.7:
install.packages("earth")
install.packages("Formula")
install.packages("plotmo")
install.packages("plotrix")
install.packages("TeachingDemos")
library(earth)
mars <- earth(wage~age+year+education+region, data = Wage, degree = 2)
summary(mars)
plot(mars)

# There are 12 terms selected (inclusive of the intercept). The generalized r-squared is 0.2908.Based upon a review of the residuals diagnostics, the residuals do not appear to be normally distributed.

#Question No.8:
install.packages("ISLR")
library(ISLR)
detach("package:gam", unload = TRUE)
install.packages("mgcv")
library(mgcv)
head(Auto)
gam.q8 <- gam(mpg~s(weight)+s(acceleration), data = Auto)
summary(gam.q8)
coef(gam.q8)
anova(gam.q8)


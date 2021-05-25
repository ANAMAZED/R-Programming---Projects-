#Uploading library 
library(ISLR)
fix(Hitters) # crossed it out to proceed
names(Hitters)
dim(Hitters)

Hitters=na.omit(Hitters) # removing rows that have missing values
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)

# Fit upto a 15 variable model 
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=15)
reg.summary=summary(regfit.full)
reg.summary

#Question 1, looking at the best model with 9 variables
coef(regfit.full,9)


#Question 2 
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
# looking at the values for rsq, cp and bic for the model 
# reg.summary$rsq 
reg.summary$adjr2
reg.summary$cp
reg.summary$bic
# to select the adjr2,bic and cp for the 6th variable model
# reg.summary$rsq[6]  # rsq
reg.summary$cp[6]    #cp
reg.summary$bic[6]   # BIC
reg.summary$adjr2[6] # adjusted Rsquare

#Question 3 

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,5) # using 5 as we have to take 5 variable model.
coef(regfit.fwd,5)
coef(regfit.bwd,5)
#  For this data, the best five variable model for subset selection and forward are identical, however forward and backward 5 variables model are different.

#Question 4 
regfit.seq=regsubsets(Salary~.,data=Hitters,nvmax=19,method="seqrep") # here i have changed the method to seqrep
summary(regfit.seq)
coef(regfit.seq,7)

#Question 5
Credit = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", row.names = 1) #Ignore the error
Credit$ID<-NULL #Remove ID
names(Credit)

regfit.full=regsubsets(Balance~.,data=Credit,nvmax=10) # using 10 variables 
reg.summary = summary(regfit.full)
reg.summary
names(reg.summary)
reg.summary$rss 
reg.summary$adjr2
reg.summary$bic

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")

# for max adjusted Rsquare
which.max(reg.summary$adjr2)
points(7,reg.summary$adjr2[7],col="red",cex=2,pch =20)

# for min rss
which.min(reg.summary$rss)
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
points(10,reg.summary$rss[10],col="red",cex=2,pch =20)

#for min BIC
which.min(reg.summary$bic)
plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type='l')
points(4,reg.summary$bic[4],col="red",cex=2,pch=20)

plot(regfit.full,scale="bic") # shows 4 variables that are selected 
plot(regfit.full,scale="adjr2") # shows the 7 variables that are selected for the final model 


# Question 6
Bodyfat= read.table("https://www.dropbox.com/s/mb1szprmqhf33pr/bodyfat.csv?dl=1",header=T,sep=",")
Bodyfat$density=NULL #duplicative and not needed
names(Bodyfat)
regfit.full=regsubsets(bodyfat~.,data=Bodyfat,nvmax=4)
reg.summary = summary(regfit.full)
reg.summary
reg.summary$adjr2[4]
reg.summary$bic[4]
reg.summary$cp[4]

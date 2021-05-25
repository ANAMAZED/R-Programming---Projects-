# RLAB 6 - PCR &PLS 

#Question 1:
library (pls)
library(ISLR)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters , scale=TRUE ,validation ="CV")
summary (pcr.fit)

#WHEN M=7 , RMSE = 345.5
#WHEN M=8 , RMSE = 347.7


#Question 2:
summary (pcr.fit)
validationplot(pcr.fit ,val.type="MSEP")
MSEP(pcr.fit)
#CV ERROR FOR M=6 is 118048



#Question 3:
Credit = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", row.names = 1)
Credit$ID<-NULL
Credit
set.seed(2)
pcr.fit=pcr(Balance~., data=Credit , scale=TRUE ,validation ="CV")
summary(pcr.fit)

#rmse for m=4 = 290.3
#rmse for m=5 = 292.1

#Question 4: 
#USING CREDIT DATA
set.seed(1)
pls.fit=plsr(Balance~.,data=Credit,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
#MSE @ M=5 is 108.1
#MSE @ M=6 IS 108.8

#USING HITTERS DATA 
set.seed(1)
pls.fit=plsr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
#MSE @ M=5 is 338.9
#MSE @ M=6 IS 340.1

#Question 5 
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset=train, scale=TRUE, validation ="CV")
validationplot(pls.fit ,val.type="MSEP")
MSEP(pls.fit)
# msep for m=2 is 108837
# msep for m=3 is 108112


#Question 6 
#using credit data:
Credit = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv", row.names = 1)
Credit$ID<-NULL
set.seed(5900)
pls.fit=plsr(Balance~.,data=Credit,subset=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit ,val.type="RMSEP")
validationplot(pls.fit ,val.type="MSEP")

# when m=2, rmse = 189.1 so mse = (189.1)^2 = 35,758.81
#RMSE IS LOWEST WHEN 9 COMPONENTS ARE USED i.e. 109.3 , MSE = (109.3)^2 = 11946.49
#RMSE IS 110.9 AND MSE IS 12298.81 WHEN COMPONENTS 4. 
# THERE IS NOT MUCH DIFFERENCE AFTER COMPONENT 4 according to the plot so I am selecting 4. 

#Question 7:
plot(pls.fit,"validation",estimate=c("train","CV"),val.type="R2",legendpos="bottomright")
#According to plot max r2 is at comp4, it does increase after 9 but a very insignificant increase so comp4 has maximum r2

#Question 8:
pls.fit$loadings
#Variables loaded include INCOME,LIMIT, RATING, EDUCATION,STUDENTYES, ETHNICITYASIAN


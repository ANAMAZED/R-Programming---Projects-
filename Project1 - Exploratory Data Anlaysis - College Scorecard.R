# EDA PROJECT 1 - ADTA 5410

#Install packages
install.packages("tidyverse")
install.packages("ISLR")
install.packages("MASS")
install.packages("dplyr")
#Load libraries
library(tidyverse) 
library(MASS) 
library(ISLR)
library(DPLYR)

#Load dataset "Most-Recent-Cohorts-All-Data-Elements.csv"
mydata = read.csv("C:/Users/anama/OneDrive/Desktop/ADTA 5410/Most-Recent-Cohorts-All-Data-Elements.csv")


#MODIFIED DATASET FOR ANALYSIS 
#CREATING NEW DATASET BASED ON QUESTIONS
#ND stands for new dataset
ND= data.frame(mydata$CONTROL,mydata$MAIN, mydata$SCUGFFN, mydata$UG12MN,  mydata$PREDDEG, mydata$NUMBRANCH, mydata$PCIP14, mydata$PCIP03, mydata$PCIP15, mydata$PCIP40, mydata$PCIP52, mydata$PCIP11, mydata$PCIP24)

# ANALYSING THE STRUCTURE OF THE DATASET SELECTED:
str(ND)

#ANALYSING SUMMARY OF THE DATASET:
summary(ND)

summary(mydata$CONTROL)
sd(mydata$CONTROL, na.rm = FALSE)
var(mydata$CONTROL)


summary(mydata$MAIN)
sd(mydata$MAIN, na.rm = FALSE)
var(mydata$MAIN)

summary(mydata$NUMBRANCH)
sd(mydata$NUMBRANCH, na.rm = FALSE)
var(mydata$NUMBRANCH)

summary(mydata$PREDDEG)
sd(mydata$PREDDEG, na.rm = FALSE)
var(mydata$PREDDEG)



#CHECKING FOR NULL VALUES, TOTAL IS 6086 IN EACH COLUMN SO 50% MEANS NULL VALUES SHOULD BE LESS THAN 3403
sum(mydata$CONTROL=="NULL")
sum(mydata$AVGFACSAL=="NULL")
sum(mydata$MAIN=="NULL")
#sum(mydata$ADM_RATE=="NULL")
sum(mydata$SCUGFFN=="NULL")
sum(mydata$UG12MN=="NULL")
sum(mydata$PREDDEG=="NULL")
sum(mydata$NUMBRANCH=="NULL")
sum(mydata$PCIP14=="NULL")
sum(mydata$PCIP03=="NULL")
sum(mydata$PCIP15=="NULL")
sum(mydata$PCIP40=="NULL")
sum(mydata$PCIP52=="NULL")
sum(mydata$PCIP11=="NULL")
sum(mydata$PCIP24=="NULL")
sum(mydata$AVGFACSAL=="NULL")

#AFTER RUNNING THE CODES I CAN SEE THAT ADMISSION RATE HAS 4800 VALUES SO I WILL IGNORE IT.
#FTFTPCFLOAN IS 1066
# G12MN IS 4734 SO I WILL IGNORE THIS ONE TOO 
#SCUGFFN IS 782


#To identify the outliers, create box plot - integers :
install.packages("pacman")
library(pacman)
install.packages(("ggplot2"))
library(ggplot2)
boxplot(mydata$CONTROL, horizontal = TRUE)
boxplot.stats(mydata$CONTROL)

boxplot.stats(mydata$MAIN)

boxplot.stats(mydata$CONTROL)
hist(mydata$CONTROL)
boxplot(mydata$MAIN, horizontal = TRUE)
boxplot(mydata$PREDDEG, horizontal = TRUE)
boxplot(mydata$NUMBRANCH, horizontal = TRUE)


#Creating boxplot of characters by converting them to integers
S <- strtoi(mydata$SCUGFFN)
boxplot(S, horizontal = TRUE)
boxplot.stats(mydata$SCUGFFN)

R <- as.numeric(mydata$SCUGFFN)
boxplot(R, horizontal = TRUE)
boxplot.stats(R)
summary(R)
UG <- strtoi(mydata$UG12MN)
boxplot(UG, horizontal = TRUE)
U<- as.numeric(mydata$UG12MN)
boxplot(U, horizontal = TRUE)


PRED <- strtoi(mydata$PREDDEG)
boxplot(PRED, horizontal = TRUE)

P14 <- as.numeric(mydata$PCIP14)
boxplot(P14, horizontal = TRUE)
boxplot.stats(mydata$P14)

P03 <- as.numeric(mydata$PCIP03)
boxplot(P03, horizontal = TRUE)

P15 <- strtoi(mydata$PCIP15)
P15<-as.numeric(mydata$PCIP15)
boxplot(P15, horizontal = TRUE)

P40 <- as.numeric(mydata$PCIP40)
boxplot(P40, horizontal = TRUE)

P52 <- as.numeric(mydata$PCIP52)
boxplot(P52, horizontal = TRUE)

P11 <- as.numeric(mydata$PCIP11)
boxplot(P11, horizontal = TRUE)

P24 <- as.numeric(mydata$PCIP24)
boxplot(P24, horizontal = TRUE)

AS <- as.numeric(mydata$AVGFACSAL)
boxplot(AS, horizontal = TRUE)


ggplot(ND, aes(mydata$CONTROL)) + 
  geom_bar(fill = "blue") 
ggplot(ND, aes(mydata$MAIN)) + 
  geom_bar(fill = "YELLOW") 


ggplot(ND, aes(x = P03, y = mydata$CONTROL)) + 
  geom_point(colour = "red") 
ggplot(ND, aes(x = P14, y = mydata$CONTROL)) + 
  geom_point(colour = "BLUE") 
ggplot(ND, aes(x = P15, y = mydata$CONTROL)) + 
  geom_point(colour = "green") 
ggplot(ND, aes(x = P11, y = mydata$CONTROL)) + 
  geom_point(colour = "red") 
ggplot(ND, aes(x = P24, y = mydata$CONTROL)) + 
  geom_point(colour = "yellow") 
ggplot(ND, aes(x = P52, y = mydata$CONTROL)) + 
  geom_point(colour = "purple") 


ggplot(ND, aes(x = U, y = mydata$MAIN)) + 
  geom_point(colour = "purple") 

ggplot(ND, aes(x = U, y = mydata$NUMBRANCH)) + 
  geom_point(colour = "GREEN") 

ggplot(ND, aes(x = mydata$PREDDEG, y = mydata$MAIN)) + 
  geom_point(colour = "purple") 

ggplot(ND, aes(x = mydata$PCIP03, y = mydata$CONTROL)) + 
  geom_point(colour = "purple") 

ggplot(ND, aes(x = R, y = mydata$CONTROL)) + 
  geom_point(colour = "BLUE") 


#ggplot(ND, aes(mydata$PCIP03, fill=mydata$CONTROL)) + 
  geom_bar() 
  
ggplot(ND, aes(x = mydata$MAIN, y = mydata$CONTROL)) + 
    geom_point(colour = "blue") 
  
  
ggplot(ND, aes(x = mydata$PREDDEG, y = mydata$CONTROL)) + 
  geom_point(colour = "red") 


ggplot(ND, aes(x = mydata$NUMBRANCH, y = mydata$CONTROL)) + 
  geom_point(colour = "green") 


  

ggplot(ND, aes(x = mydata$CONTROL, fill = mydata$PCIP15)) + 
    geom_bar(position = "dodge") + theme(axis.text.x = element_text(angle = 90))

# creating line and point
ggplot(ND, aes(x = mydata$CONTROL, y = mydata$NUMBRANCH)) + geom_line()+ geom_point()

table(mydata$CONTROL)
mean(mydata$CONTROL)
median(mydata$CONTROL)
mode(mydata$CONTROL)
hist(mydata$CONTROL)
hist(mydata$AVGFACSAL)

library(gcookbook)
plot(mydata$CONTROL, mydata$NUMBRANCH)
color <- c("red", "yellow", "green", "violet", "orange", "blue", "pink", "cyan")
pie(mydata$CONTROL)

ggplot(mydata, aes(x=CONTROL, y = PCIP03))

qplot(PCIP03, geom = "histogram", fill = CONTROL , bins = 30, data = mydata)

       
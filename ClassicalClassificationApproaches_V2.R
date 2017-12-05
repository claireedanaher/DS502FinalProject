#SET WORKING DIRECTORY
setwd("C:/WPI/DS502/FinalProject/")

#IMPORT DATA
library(readxl)
library(readr)
library(ISLR)
library(MASS)
library(class)
library(FactoMineR)
library(ggplot2)
library(leaps)
library(boot)
library(bestglm)
library('reshape2')

#prints variable names

data <- read_csv("analysisDF.csv")
DF=data.frame(data)
DF$remove[DF$in_32==0 | DF$in_33==0 |DF$in_34==0 | DF$in_34==0] <- 1
DF=DF[DF$remove==0,]
DF1=DF[c(11:60,64)]
DF1$improve <- as.factor(DF1$improve)

DF2=DF1
DF2$money <- apply(DF2[,1:6], 1, sum)
DF2$health <- apply(DF2[,7:15], 1, sum)
DF2$housing <- apply(DF2[,16:27], 1, sum)
DF2$edu <- apply(DF2[,28:38], 1, sum)
DF2$org <- apply(DF2[,39:42], 1, sum)
DF2$motive <- apply(DF2[,43:50], 1, sum)
DF2 <- DF2[,51:57]


for(i in 1:50) DF1[,i]=as.factor(DF1[,i])


set.seed(3)
quarter <- sample(dim(DF)[1], dim(DF)[1]/4)
DF1_train <- DF1[-quarter,]
DF1_test <- DF1[quarter,]
improve.test=DF1_test[,c(51)]

DF2_train <- DF2[-quarter,]
DF2_test <- DF2[quarter,]

##########################################################################################
#
#Logistic Regression 50 Dims
#
##########################################################################################


#################LOGISTIC REGRESSION
glm.fits=glm(improve~., data=DF1, subset=-quarter, family=binomial)
glm.probs=predict(glm.fits,DF1_test, type="response")
glm.pred=rep(0, 506)
glm.pred[glm.probs>.5]=1
table(glm.pred,improve.test) 

mean(glm.pred==improve.test)



probs=c(0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1)
val=rep(0,10)
thresh=data.frame(probs,val,val,val,val)

#################THRESHOLD SELECTION
glm.fits=glm(improve~., data=DF1, subset=-quarter, family=binomial)
glm.probs=predict(glm.fits,DF1_test, type="response")

for(i in 1:10){
  glm.pred=rep(0, 506)
  glm.pred[glm.probs>thresh[i,c(1)]]=1
  x=table(glm.pred,improve.test) 
  thresh[i,2]=x[1]
  thresh[i,3]=x[4]
  thresh[i,4]=x[1]/33
  thresh[i,5]=x[4]/473
  table(glm.pred,improve.test) 
  mean(glm.pred==improve.test)}

thresh[1:10,]

ggplot(thresh, aes(probs)) + 
  geom_line(aes(y = val.3, colour = "Improve")) + 
  geom_line(aes(y = val.2, colour = "Not Improve"))+
  labs( 
       y="True Positives", 
       x="Probability Cutoff")


#################LOGISTIC REGRESSION-  Optimal Threshold
glm.fits=glm(improve~., data=DF1, subset=-quarter, family=binomial)
summary(glm.fits)
glm.probs=predict(glm.fits,DF1_test, type="response")
glm.pred=rep(0, 506)
glm.pred[glm.probs>.91]=1
table(glm.pred,improve.test) 

mean(glm.pred==improve.test)


#############################################################################################







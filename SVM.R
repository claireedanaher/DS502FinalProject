setwd("C:/users/Russ/Documents/WPI Courses/DS 502/DS502 Project")
library(readxl)
library(readr)
library(e1071)
library (ROCR)

DF = read_csv("analysisDF.csv")

attach(DF)

ix = 7:57
DF[ix] = lapply(DF[ix], as.factor)
set.seed(3)
DF$remove[DF$in_32==0 | DF$in_33==0 |DF$in_34==0 | DF$in_34==0] <- 1
DF=DF[DF$remove==0,]
test=sample(1:nrow(DF),nrow(DF)/4)
DF_train=DF[-test,]
DF_test=DF[test,]

#DF$improve=factor(DF$improve)

subdata_train=subset(DF_train,select=c(7:57))
subdata_test=subset(DF_test,select=c(7:57))
improve_train = DF_train$improve
improve_test=DF_test$improve
traindat = subset(DF_train,select=c(7:57))
testdat = subset(DF_test,select=c(7:57))

tune.lin=tune(svm,improve~.,data=subdata_train,kernel="linear",
    ranges=list(cost=2^(-2:5)))

tune.poly = tune(svm, improve~., data=subdata_train, kernel="poly",
                 ranges=list(cost=2^(-2:10), degree = c(2:10)))

tune.rad = tune(svm,improve~.,data=subdata_train,kernel="radial",
    ranges=list(cost=2^(-2:5), gamma=2^(-5:3)))




bestlin = tune.lin$best.model
bestrad = tune.rad$best.model
bestpoly = tune.poly$best.model

print(bestlin)
print(bestrad)
print(bestpoly)

pred=predict(bestlin,subdata_train)
table(pred,subdata_train$improve)
mean(subdata_train$improve == pred)

pred=predict(bestlin,subdata_test)
table(pred,subdata_test$improve)
mean(subdata_test$improve == pred)

pred=predict(bestpoly,subdata_train)
table(pred, subdata_train$improve)
mean(subdata_train$improve == pred)

pred=predict(bestpoly,subdata_test)
table(pred, subdata_test$improve)
mean(subdata_test$improve == pred)

pred=predict(bestrad,subdata_train)
table(pred,subdata_train$improve)
mean(subdata_train$improve == pred)

pred=predict(bestrad,subdata_test)
table(pred,subdata_test$improve)
mean(subdata_test$improve == pred)

rocplot =function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)}

bestlin.ROC=svm(improve~., data=subdata_train, kernel ="linear",
               cost=32, decision.values=T)

bestpoly.ROC=svm(improve~., data=subdata_train, kernel ="poly",
                cost=32, d =8, decision.values=T)

bestrad.ROC=svm(improve~., data=subdata_train, kernel ="rad",
                cost=2, gamma = 1, decision.values=T)


fittedlin=attributes(predict(bestlin.ROC,subdata_train, 
                             decision.values=TRUE))$decision.values

fittedpoly = attributes(predict(bestpoly.ROC,subdata_train, 
                                decision.values=TRUE))$decision.values

fittedrad = attributes(predict(bestrad.ROC,subdata_train, 
                               decision.values=TRUE))$decision.value

rocplot(fittedlin,as.numeric(subdata_train$improve == 1), main ="Training Data", lwd = 2)
rocplot(fittedpoly,as.numeric(subdata_train$improve == 1), add=T,col="red", lwd = 2)
rocplot(fittedrad,as.numeric(subdata_train$improve == 1), add=T,col="blue", lwd = 2)
names = c("linear kernel","polynomial kernel","radial kernel")
legend('bottomright', names, lty=1, col=c('black', 'red', 'blue'), bty='n', cex=.75)

fittedlin=attributes(predict(bestlin.ROC,subdata_test, 
                          decision.values=TRUE))$decision.values

fittedpoly = attributes(predict(bestpoly.ROC,subdata_test, 
                                decision.values=TRUE))$decision.values

fittedrad = attributes(predict(bestrad.ROC,subdata_test, 
                               decision.values=TRUE))$decision.value

rocplot(fittedlin,as.numeric(subdata_test$improve == 1), main ="Test Data", lwd = 2)
rocplot(fittedpoly,as.numeric(subdata_test$improve == 1), add=T,col="red", lwd = 2)
rocplot(fittedrad,as.numeric(subdata_test$improve == 1), add=T,col="blue", lwd = 2)
names = c("linear kernel","polynomial kernel","radial kernel")
legend('bottomright', names, lty=1, col=c('black', 'red', 'blue'), bty='n', cex=.75)





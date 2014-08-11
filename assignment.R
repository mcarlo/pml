# assignment
setwd("~/GitHub/pml")
training <- read.csv("~/GitHub/pml/pml-training.csv")
testing <- read.csv("~/GitHub/pml/pml-testing.csv")
colnames(training)

library(knitr); library(caret); library(party);library(ElemStatLearn); 
library(MASS);library(plyr); library(gbm); library(survival); library(splines);
library(randomForest)

allColumns <- colnames(training)

# id columns that are not NA in testing data set
availColumns <- cbind(colnames(training),apply(testing, 2, max))
availColumns <- availColumns[is.na(availColumns[,2])==FALSE ,1]
availTrain <- training[, cbind(availColumns)[-c(3, 4, 5, 6)]]
availTest <- testing[, colnames(testing) %in% colnames(availTrain)]


trainRows <- nrow(availTrain)
predictions <- 100
testRows <- nrow(availTest)

predictionMatrix <- matrix(rep(0, 100), nrow = 20, ncol = 5)
rm(rfApred)

rfApred <- matrix(rep("",testRows*5*predictions),nrow=testRows,ncol=5*predictions)
for (i in 1:predictions){
  endCol <- 5*i
  beginCol <- endCol - 4
  set.seed(i)
  sample.split <- sample(1:trainRows,size=5000,replace=F)
  trainSample <- availTrain[sample.split,]
  rfA <- randomForest(as.factor(classe) ~ . , data = trainSample, ntree = 100,type="class")
  rfApred[, beginCol:endCol] <- predict(rfA,newdata=availTest,type="class")
}
rfApred
rfAmean <- matrix(rep(0.0, predictions), nrow = 20, ncol = 5)
for (j in 1:5){
  predCols <- 5*(1:predictions)-5 + j
  rfAmean[, j] <- apply(rfApred[, predCols], 1, mean)
}
apply(rfApred[, predCols-4], 1, mean)
rfAmean

#Q3
#1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

#1. Subset the data to a training set and testing set based on the Case variable in the data set. 
train <- subset(segmentationOriginal, Case = "Train")
train <- train [, -2]
test <- subset(segmentationOriginal, Case = "Test")
test <- test [, -2]

#1a
set.seed(125)
modFit1a <- train(Class ~ .,data=train, method = "rpart")
print(modFit1a$finalModel)
preds1a <- predict(modFit1a$finalModel)

set.seed(125)
modFit1b <- train(Class ~ .,data=train[,-2], method = "rpart")
print(modFit1b$finalModel)


#2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
set.seed(125)
modFit <- train(Class ~ .,data=segmentationOriginal, subset = Case=="Train", method = "rpart")
print(modFit$finalModel)
plot(modFit$finalModel)
preds <- predict(modFit$finalModel)

str(modFit)
print(modFit$finalModel)
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=1.)

library(rattle)
fancyRpartPlot(modFit$finalModel)
testA <- test[1,]
testA$TotalIntench2 = 23000; testA$FiberWidthCh1 = 10; testA$PerimStatusCh1=2;
predict(modFit$finalModel, newdata = testA, type = "class")

testB <- test[2,]
testB$TotalIntench2 = 50000; testB$FiberWidthCh1 = 10; testB$VarIntenCh4 = 100 
predict(modFit$finalModel, newdata = testB, type = "class")

testC <- test[3,]
testC$TotalIntench2 = 57000; testC$FiberWidthCh1 = 8;testC$VarIntenCh4 = 100;
predict(modFit$finalModel, newdata = testC, type = "class")

testD <- test[4,]
testD$TotalIntench2 = NA; testD$FiberWidthCh1 = 8;testD$VarIntenCh4 = 100; testD$PerimStatusCh1=2 ;
predict(modFit$finalModel, newdata = testD, type = "class")

#3. In the final model what would be the final model prediction for cases with the following variable values:
#a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 PS
#b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 PS
#c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 PS
#d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

#3
library(pgmm)
data(olive)
olive = olive[,-1]

tree1 <- tree(Area ~ .,data=olive)
print(tree1)

train <- olive

newdata1 = as.data.frame(t(colMeans(olive)))
predict(tree1, newdata = newdata1)

train$Area <- as.factor(train$Area)
tree2 <- tree(Area ~ ., data = train)
predict(tree2, newdata = newdata1)

library(rattle)
fancyRpartPlot(tree1)

#4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
glm2 <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
              method = "glm", data = trainSA, family = "binomial")
summary(glm2)
pred1 <- predict(glm2, trainSA)
pred2 <- predict(glm2, newdata = testSA)

mad <- function(values, predictions){
  sum(((predictions > 0.5)*1) != values)/length(values)
}

mad(trainSA$chd, pred1)
mad(testSA$chd, pred2)

#5
library(ElemStatLearn)
train <- vowel.train
test <- vowel.test

train$y <- as.factor(train$y)
test$y <- as.factor(test$y)
set.seed(33833)
library(randomForest)
rf1 <- randomForest(y ~ ., data = train, importance = T)
rownames(importance(rf1))[order(importance(rf1))]
summary(rf1)
library(caret)
varImp(rf1)
str(importance(rf1))
importance(rf1)[order(-importance(rf1)[,13]),13]

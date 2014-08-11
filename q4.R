# Quiz 4
library(ElemStatLearn);library(caret);library(gbm)
training <- vowel.train
testing <- vowel.test

training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)

set.seed(33833)
mod1 <- train(y ~.,method="rf",data=training)
mod2 <- train(y ~.,method="gbm",data=training)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
confusionMatrix(pred1, testing$y)$overall
confusionMatrix(pred2, testing$y)$overall
confusionMatrix(pred1, pred2)$overall

#2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod1 <- train(diagnosis ~.,method="rf",data=training)
mod2 <- train(diagnosis ~.,method="gbm",data=training)
mod3 <- train(diagnosis ~.,method="lda",data=training)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing);
pred3 <- predict(mod3,testing)

confusionMatrix(pred1, testing$diagnosis)$overall[1]
confusionMatrix(pred2, testing$diagnosis)$overall[1]
confusionMatrix(pred3, testing$diagnosis)$overall[1]


predDF <- data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)
combModFit <- train(diagnosis ~.,method="rf",data=predDF)
combPred <- predict(combModFit,predDF)

confusionMatrix(combPred, testing$diagnosis)$overall[1]

#3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
library(lars)
covnames <- names(training[,-9])
lasso.fit <- lars(as.matrix(training[,-9]), training$CompressiveStrength, type="lasso", trace=TRUE)

#png(file="Plots/selection-plots-04.png", width=432, height=432, pointsize=8) 
plot(lasso.fit, breaks=FALSE)
legend("topleft", covnames, pch=8, lty=1:length(covnames), col=1:length(covnames))
dev.off()

# this plots the cross validation curve
png(file="Plots/selection-plots-05.png", width=432, height=432, pointsize=12) 
lasso.cv <- cv.lars(as.matrix(training[,-9]), training$CompressiveStrength, K=10, type="lasso", trace=TRUE)
dev.off()

#4
library(lubridate)  # For year() function below
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
tstest <- ts(testing$visitsTumblr)

library(forecast)
bats1 <- bats(tstrain)
predBats1 <- forecast.bats(bats1, h = 235, level = .95)
predAccurate <- 1*(predBats1$lower <= tstest)
(predBats1$upper >= tstest)
mean(predAccurate)
plot(predBats1$lower)
plot(predBats1$upper)
plot(tstest)

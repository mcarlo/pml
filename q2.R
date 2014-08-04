# Quiz 2
#Q1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

#Q2 
library(Hmisc)
data(concrete)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

mixtures$mixRows <- as.numeric(rownames(mixtures))
plot(as.numeric(rownames(mixtures)), mixtures$CompressiveStrength)

lmFlyAsh <- lm(CompressiveStrength ~ mixRows + FlyAsh, mixtures)
lmAge <- lm(CompressiveStrength ~ mixRows + Age, mixtures)
lmMix <- lm(CompressiveStrength ~ ., mixtures)
summary(lmFlyAsh)
summary(lmAge)
summary(lmMix)

#Q3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(mixtures$Superplasticizer)
summary(mixtures$Superplasticizer)

#Q4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

colnames(training)
q4var <- training[,58:69]

pca4 <- preProcess(q4var, method = "pca", thresh = 0.80)
dimnames(pca4$rotation)[[2]]
str(pca4)
colnames(pca4)
pca4values <- as.matrix(q4var) %*% as.matrix(pca4$rotation)
colnames(pca4values) <- dimnames(pca4$rotation)[[2]]
head(pca4values)


#Q5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

train1 <- training
train2 <- data.frame(cbind(training$diagnosis, pca4values))
colnames(train2)[1] <- "diagnosis"
summary(train2)
glm1 <- glm(diagnosis ~ ., train1, family = "binomial")
glm2 <- glm(diagnosis-1 ~ ., train2, family = "binomial")

?predict
pred1 <- predict.glm(glm1, newdata = testing, type = "response")
table(testing$diagnosis, pred1)

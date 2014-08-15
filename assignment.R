# assignment
setwd("~/GitHub/pml")
training <- read.csv("~/GitHub/pml/pml-training.csv")
testing <- read.csv("~/GitHub/pml/pml-testing.csv")
colnames(training)

library(knitr); library(caret); library(randomForest); library(gbm);
#library(party);library(ElemStatLearn); 
library(MASS);library(plyr); library(survival); library(splines);


allColumns <- colnames(training)

# id columns that are not NA in testing data set
availColumns <- cbind(colnames(training),apply(testing, 2, max))
availColumns <- availColumns[is.na(availColumns[,2])==FALSE ,1]
availTrain <- training[, cbind(availColumns)[-c(1,3:6)]]
availTest <- testing[, colnames(testing) %in% colnames(availTrain)]

# Not run for rmd file
#modGBM <- train(classe ~ ., method="gbm",data=availTrain,verbose=FALSE)
#modRF <- train(classe ~ ., method="rf",data=availTrain,verbose=FALSE,
               type = "prob")

answers <- predict(modGBM, newdata = availTest)
#B A B A A E D B A A B C B A E E A B B B
#B A B A A E D B A A B C B A E E A B B B

varImp(availRF)
modRF
modGBM

predict(modRF, newdata = availTest) # identical to GBM
#getwd()

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    if (i < 10){
      prefix = paste("problem_id_","0",sep="")
      filename = paste0(prefix,i,".txt")
    }
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

sampleClasse <- createDataPartition(y=availTrain$classe,
                                           p=0.2, list=FALSE)
sampleC <- availTrain[sampleClasse,]
sampleUser <- createDataPartition(y=sampleC$user_name,
                                  p=0.25, list=FALSE)
sampleU <- sampleC[sampleUser,]
sampleRF <- train(classe ~ ., method="rf",data=sampleU[,c(1:10,55)],verbose=FALSE)
sampleRF
sampleRFa <- train(classe ~ ., method="rf",data=sampleU[,c(11:27,55)],verbose=FALSE)
sampleRFa
sampleRFb <- train(classe ~ ., method="rf",data=sampleU[,c(28:41,55)],verbose=FALSE)
sampleRFb
sampleRFc <- train(classe ~ ., method="rf",data=sampleU[,c(42:54,55)],verbose=FALSE)
sampleRFc
availRF <- train(classe ~ ., method="rf",data=availTrain[sampleClasse,c(1:10,55)],verbose=FALSE)
availRF
system.time()
Overall
num_window        100.0000
roll_belt          31.1554
yaw_belt           22.8421
pitch_belt         22.7179
total_accel_belt    7.0694
gyros_belt_z        4.3541
user_nameeurico     4.3255
accel_belt_x        3.8776
user_namejeremy     3.5353
gyros_belt_y        2.1700
gyros_belt_x        2.1581
user_namecharles    0.9833
user_namecarlitos   0.7608
user_namepedro      0.0000

#pml_write_files(answers)
ls()
read
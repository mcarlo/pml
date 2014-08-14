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

#pml_write_files(answers)
ls()
read
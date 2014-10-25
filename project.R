# Course Project, 10/24/2014
#
# The goal of this project is to predict the manner in which they did the exercise. 
# This is the "classe" variable in the training set. 
#
# We should create a report describing how you built our model, how you used cross 
# validation, what we think the expected out of sample error is, and why we made 
# the choices we did. We will also use our prediction model to predict 20 different 
# test cases.

### Preparing Data
#
# The training data for this project are available here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#
# The test data are available here:
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

### Loading Data

training.csv<-read.csv("pml-training.csv", header = TRUE, na.strings = c("#DIV/0!","NA", ""))
testing.csv<-read.csv("pml-testing.csv", header = TRUE, na.strings = c("#DIV/0!","NA", ""))
print( dim(training.csv) )
print( dim(testing.csv) )

### Cleaning Data

# remove near zero variance
library(caret)
nearZero <- nearZeroVar(training.csv, saveMetrics = TRUE)
training.csv <- training.csv[, nearZero$nzv==FALSE]
print( dim(training.csv) )
# remove NAs
training.csv<-training.csv[ , colSums(is.na(training.csv)) == 0]
# remove not relevant columns
training.csv<-training.csv[,7:ncol(training.csv)]
print( dim(training.csv) )

### Random Forest

# split data
set.seed(33833)
inTrain <- createDataPartition(training.csv$classe,p=0.7,list=FALSE)
training <- training.csv[inTrain,]
testing <- training.csv[-inTrain,]

# train model
ctrl <- trainControl(method = "cv", number = 4, allowParallel = TRUE)
modFit<-train( classe ~ ., data=training, method="rf", trControl = ctrl )
print( modFit$finalModel )

# in sample error
inError<-sum(diag(modFit$finalModel$confusion))/sum(modFit$finalModel$confusion)
print(inError)

# cross validation
testingPredict<-predict( modFit, testing )
cm<-confusionMatrix(testingPredict,testing$classe)
print( cm )

# out of sample error
outError<-sum(diag(cm$table))/sum(cm$table)
print( outError )

### Predicting 20 Test Cases
predict20 <- predict(modFit, testing.csv)
print( predict20 )

# create one file for each submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predict20)

# --- End of File ---
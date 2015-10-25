#Practical machine learning assignment

#Start by loading the necessary libraries
library(caret)
library(ggplot2)

#Import the data and clean it up a bit
pmlimport <- read.csv("pml-training.csv", header = TRUE, sep = ",")

pmldata <- pmlimport[,sapply(seq(1:length(colnames(pmlimport))),
                             function(i) is.numeric(pmlimport[,i])) & 
                            sapply(seq(1:length(colnames(pmlimport))), 
                            function(i) sum(is.na(pmlimport[,i]))==0)]

#Add the classe (result) and user-name back into the data frame
#pmldata$user_name <- pmlimport$user_name
pmldata$classe <- pmlimport$classe

#Remove the timestamps, window numbers and index
pmldata <- pmldata[,!(names(pmldata) %in% 
                        c("raw_timestamp_part_1","raw_timestamp_part_2",
                          "num_window","X"))]

#Remove the original import to free up space
rm(pmlimport)

#First we split the training data into training and test sets, 75/25
set.seed(1234)
inTrain <- createDataPartition(y=pmldata$classe, p=0.75, list = FALSE)

pmltraining <- pmldata[inTrain,]
pmltesting <- pmldata[-inTrain,]

#We will use k-fold cross-validation on the training set, with 5 folds
trainfolds <- createFolds(y=pmltraining$classe, k = 5, list = TRUE, returnTrain = TRUE)
testfolds <- createFolds(y=pmltraining$classe, k = 5, list = TRUE, returnTrain = FALSE)

#Use the different folds for checking the in-sample error
for (i in 1:length(trainfolds)){
  mod <- train(classe ~ ., data=pmltraining[trainfolds[[i]],], 
                  preProcess = c("center", "scale"), method = "qda")
  pred <- predict(mod, pmltraining[testfolds[[i]],])
  print(confusionMatrix(pred,pmltraining[testfolds[[i]],]$classe))
}

#Apply the model to the test set just once, to estimate the out-of-sample error
finalmod <- train(classe ~ ., data=pmltraining, 
                  preProcess = c("center", "scale"), method = "qda")
finalpred <- predict(finalmod, pmltesting)
print(confusionMatrix(finalpred,pmltesting$classe))

#Code to apply the model to the 20 test examples for submission

pmlfinalimport <- read.csv("pml-testing.csv", header = TRUE, sep = ",")

pmlfinaldata <- pmlfinalimport[,sapply(seq(1:length(colnames(pmlfinalimport))),
                             function(i) is.numeric(pmlfinalimport[,i])) & 
                       sapply(seq(1:length(colnames(pmlfinalimport))), 
                              function(i) sum(is.na(pmlfinalimport[,i]))==0)]

#Add the classe (result) and user-name back into the data frame
#pmldata$user_name <- pmlimport$user_name
pmlfinaldata$classe <- pmlfinalimport$classe

#Remove the timestamps, window numbers and index
pmlfinaltest <- pmlfinaldata[,!(names(pmlfinaldata) %in% 
                        c("raw_timestamp_part_1","raw_timestamp_part_2",
                          "num_window","X"))]

submitpred <- predict(finalmod, pmlfinaltest)
answers <- as.vector(submitpred)

#####################################################################
#
# File Name: data_workup.R
# Author: Jeremy Beck
# Date: 11/14/2015
#
# Description:  This script contains sections corresponding
#     to data loading and formatting, EDA, and modeling to 
#     predict the activity class of a Human Activity Recognition
#     data set. 
#
###################################################################

library(caret)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

# Downlaod and load the training set
{
  download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv', 
                './data/pml-training.csv', method='curl') 
  
  train_data <- read.csv('./data/pml-training.csv', header=T, quote='"', na.strings=c("NA","","#DIV/0!"))
}

# Let's look at the class to make sure things aren't imbalanced, which
# would affect our sampling decisions
#
# Then we'll split out the test set. 
{
  table(train_data$classe)
  #Nope, classes are fairly balanced. Let's just split it.   

  set.seed(42)
  training.idx <- createDataPartition(train_data$X, p=0.75, list=F)
  
  train <- train_data[training.idx,]
  test_set <- train_data[-training.idx,]
  
  #belt_vars <- grepl('belt', names(train))
  #arm_vars <- grepl('arm', names(train))
  #dumbbell_vars <- grepl('dumbbell', names(train))
  #forearm_vars <- grepl('forearm', names(train))
  
  
}


# Basic Variable Formatting and Reduction
{
  # Get Rid of Zero-Variance Predictors
  
  # Remove columns with 80% missing
  train <- train[lapply(train, function(x){sum(is.na(x)) / length(x)} ) < 0.2 ]
  
  # Remove columns with near or zero variance
  train.nzv <- nearZeroVar(train)
  train.filtered <- train[-train.nzv]
  
  # And we get rid of the "key" columns to identify participants
  # I can't find a detailed codebook to tell me what "num_window" is, but I am suspicious of it given it's placement in
  # file with the other ID vars. For the time being, we'll exclude it from the analysis.  Better safe than sorry!
  key.cols <- grep("^X$|user_name|raw_timestamp_part_1|raw_timestamp_part_2|cvtd_timestamp|num_window",names(train.filtered))
  
  train.filtered <- train.filtered[-key.cols]
  # Let's look at the correlation betweeen predictors
  trainCor <- cor(train.filtered[-grep("classe",names(train.filtered))])
  
  names(train.filtered)[findCorrelation(trainCor, cutoff = .75)]
  #summary(trainingCor[upper.tri(trainingCor)])
  
  #train.filtered <- train.filtered[-findCorrelation(trainCor, cutoff=0.75)]
}


# Let's Do Some Cross Validation
{

  # We are going to stick with trees for this work.  Since we are dealing with classification, we don't need to do a ton of
  # preprocessing on the data.  The way that CART and Random Forests derive splits makes them robust to non-normality and 
  # outliers. In fact, trees will show no response to any sort of monotonic transformations in predictor values.  (They can 
  # make you a bit lazy that way.)

  # We will still do Cross-Validation to estimate the stability of the model to repeated random samplings. 

  ctrl <- trainControl(method="cv", number=10)

  rf_mod <- train(classe ~ ., data=train.filtered, method='rf', ntree=100, trControl=ctrl)
  
  # Let's take a look at our models now:
  
  rf_mod
  
  # Wow, the accuracy is 0.995 and the SD is 0.0018.  That's a highly accurate model, and it does not seem to be affected 
  # by the sample the model was trained on in CV.
  # Now Let's confirm with the confusion matrix
  
  confusionMatrix(rf_mod)

}


# Let's Predict on the Test Set
{
  test_preds <- predict(rf_mod, newdata=test_set)

  confusionMatrix(test_preds, test_set$classe)
  
}


# Now We Load the Test Set
{
  download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv', 
                './data/pml-testing.csv', method='curl') 

  test_data <- read.csv('./data/pml-testing.csv', header=T, quote='"', na.strings=c("NA","","#DIV/0!"))

}

# And We Run some Test Predictions
{
  testset_preds <- predict(rf_mod, newdata=test_data)

  pml_write_files(as.character(testset_preds))
  
}
## API-222 
## Code by Arjun Bisen
## Prediction Competition
## Harvard Kennedy School

rm(list=ls())

# Common libraries
library(ISLR)
library(glmnet)
library(class)
library(pls)
library(dplyr)
library(tree)
library(survival)
library(fastDummies)
library(randomForest)
library(nnet)


# Importing dataframe
training.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/training_data.csv")

test.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/test_df.csv")


head(training.data, 3)
dim(training.data)

# Find missing rows 
find_missing_rows <- function(df){
  return(nrow(df[!complete.cases(df),]))
}
find_missing_rows(training.data)

# Finding columns with missing data
str(training.data)

for (col in colnames(training.data))
  print(paste(col, sum(is.na(training.data[, col]))))


######################################################
##### ADDRESSING SMALL AMOUNTS OF MISSING VALUES #####
# for columns that have small amount of missing values (meaneduc and SQBmeaned)
# we impute the population level summary statistics
######################################################

# Removing ID columns and those with many missing values
clean_arj_data <- function(df){
  arj_data <- df
  arj_data$v2a1 <- NULL
  arj_data$v18q1 <- NULL
  arj_data$rez_esc <- NULL
  arj_data$Id <- NULL
  arj_data$idhogar <- NULL
  
  ## for meaneduc ##
  # calculate mean of meaneduc (removing entries with missing values)
  meaneduc_mean <- mean(arj_data$meaneduc,na.rm=TRUE)
  
  #calculate median of meaneduc (removing entries with missing values)
  meaneduc_median <- median(arj_data$meaneduc,na.rm=TRUE)
  
  # compare the two
  sprintf("Mean of 'meaneduc': %f", meaneduc_mean)
  sprintf("Median of 'meaneduc': %f", meaneduc_median)
  
  # they seem close but let's still check for outliers just to be sure 
  plot(arj_data$meaneduc)
  
  # they seem pretty close and it doesn't look like there
  # are outliers having an outsized effect, so let's use the median for consistency
  arj_data$meaneduc[is.na(arj_data$meaneduc)] <- meaneduc_median
  
  ## for SQBmeaned ##
  # calculate mean of SQBmeaned (removing entries with missing values)
  SQBmeaned_mean <- mean(arj_data$SQBmeaned,na.rm=TRUE)
  
  #calculate median of SQBmeaned (removing entries with missing values)
  SQBmeaned_median <- median(arj_data$SQBmeaned,na.rm=TRUE)
  
  # compare the two
  sprintf("Mean of 'SQBmeaned': %f", SQBmeaned_mean)
  sprintf("Median of 'SQBmeaned': %f", SQBmeaned_median)
  
  # they seem farther apart so let's visualize and check for outliers
  plot(arj_data$SQBmeaned)
  
  # there's a few very big outliers, and the mean isn't robust to outliers
  # so lets replace the missing values with median instead
  arj_data$SQBmeaned[is.na(arj_data$SQBmeaned)] <- SQBmeaned_median
  
  # check to make sure we do not have any missing values anymore
  stopifnot(find_missing_rows(arj_data)==0)
  
  return(arj_data)
}

clean_data <- clean_arj_data(training.data)
clean_data$Target <- as.factor(clean_data$Target)
true_test <- clean_arj_data(test.data)


# Split data into training and test
train<- sample(seq(nrow(clean_data)),
                              floor(nrow(clean_data) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(clean_data)) %in% train))

# Training and testing my random forest
forest_model <- randomForest(Target~.,data= clean_data, importance = TRUE)
forest_predict <- predict(forest_model,clean_data[test,])


# F1 score generator
f1_score <- function(predicted_y, true_y) {
  library(dplyr)
  num_unique_y      <- length(unique(true_y))
  scores            <- vector(length = num_unique_y, mode = "double")
  
  for (i in 1:num_unique_y) {
    trans_pred      <- as.numeric(predicted_y == i)
    trans_true      <- as.numeric(true_y == i)
    df              <- cbind.data.frame(trans_pred, trans_true)
    colnames(df)    <- c("pred", "true")
    df              <- df %>%
      mutate(true_pos = ((pred == 1) & (true == 1)),
             true_neg = ((pred == 0) & (true == 0)),
             false_pos = ((pred == 1) & (true == 0)),
             false_neg = ((pred == 0) & (true == 1))) %>%
      summarise(true_pos = sum(true_pos),
                false_pos = sum(false_pos),
                false_neg = sum(false_neg))
    scores[i]       <- 2 * df$true_pos / (2 * df$true_pos + 
                                            df$false_neg + 
                                            df$false_pos)
    
  }
  F1                <- mean(scores)
  return(F1)
}

print(f1_score(forest_predict, Y_test))
x  

################### Trying Neural Nets #########################

?nnet
nn_model <- nnet(as.factor(Y_train)~., data = X_train, size=15, maxit=10000, MaxNWts=10000)
nnpredict_train <- as.numeric(predict(nn_model, X_train, type = "class"))
nnpredict_test <- as.numeric(predict(nn_model, X_test, type = "class"))
print(f1_score(nnpredict_train, Y_train))
print(f1_score(nnpredict_test, Y_test))

################################################################

# Now to train the random forest model on my full dataset

X_train_full = clean_data[,-138]
Y_train_full = clean_data[,138]
Y_train_full <- as.factor(Y_train_full)
colnames(clean_data)
forest_model <- randomForest(Target~.,data=clean_data[train, ], importance = TRUE)
################################################################ 
###################  Clean test data  ##########################
################################################################

#test.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/test_df.csv")

all(colnames(true_test)==colnames(clean_data[,-138]))

test_types <- c()
for(col in colnames(true_test)){
  test_types <- append(test_types, class(true_test[,col])) 
}

training_types <- c()
for(col in colnames(clean_data[,-138])){
  training_types <- append(training_types, class(clean_data[,col]))
}

colnames(training.data)[96]

forest_predict <- predict(forest_model, true_test)
View(forest_predict)



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
library(neuralnet)

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

# Removing ID columns and those with many missing values

training.data$v2a1 = NULL
training.data$v18q1 = NULL
training.data$rez_esc = NULL
training.data$Id = NULL
training.data$idhogar = NULL


######################################################
##### ADDRESSING SMALL AMOUNTS OF MISSING VALUES #####
# for columns that have small amount of missing values (meaneduc and SQBmeaned)
# we impute the population level summary statistics
######################################################

## for meaneduc ##
# calculate mean of meaneduc (removing entries with missing values)
meaneduc_mean = mean(training.data$meaneduc,na.rm=TRUE)

#calculate median of meaneduc (removing entries with missing values)
meaneduc_median = median(training.data$meaneduc,na.rm=TRUE)

# compare the two
sprintf("Mean of 'meaneduc': %f", meaneduc_mean)
sprintf("Median of 'meaneduc': %f", meaneduc_median)

# they seem close but let's still check for outliers just to be sure 
plot(training.data$meaneduc)

# they seem pretty close and it doesn't look like there
# are outliers having an outsized effect, so let's use the median for consistency
training.data$meaneduc[is.na(training.data$meaneduc)] = meaneduc_median

## for SQBmeaned ##
# calculate mean of SQBmeaned (removing entries with missing values)
SQBmeaned_mean = mean(training.data$SQBmeaned,na.rm=TRUE)

#calculate median of SQBmeaned (removing entries with missing values)
SQBmeaned_median = median(training.data$SQBmeaned,na.rm=TRUE)

# compare the two
sprintf("Mean of 'SQBmeaned': %f", SQBmeaned_mean)
sprintf("Median of 'SQBmeaned': %f", SQBmeaned_median)

# they seem farther apart so let's visualize and check for outliers
plot(training.data$SQBmeaned)

# there's a few very big outliers, and the mean isn't robust to outliers
# so lets replace the missing values with median instead
training.data$SQBmeaned[is.na(training.data$SQBmeaned)] = SQBmeaned_median

# check to make sure we do not have any missing values anymore
stopifnot(find_missing_rows(training.data)==0)

# Split data into training and test
train               <- sample(seq(nrow(training.data)),
                              floor(nrow(training.data) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(training.data)) %in% train))

# Separating X and Y variables in training set
X_train = training.data[train,-138]
Y_train = training.data[train, 138]
Y_train <- as.factor(Y_train)

# Separating X and Y variables in test set
X_test = training.data[test,-138]
Y_test = training.data[test, 138]
Y_test <- as.factor(Y_test)

# Running random forest
forest_model <- randomForest(Y_train~.,data= X_train, importance = TRUE)
forest_predict <- predict(forest_model,X_test)


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

?neuralnet
# sample code suggests NN's require a full formula 
features <- names(X_train)
f <- paste(features, collapse= ' + ')
f <- paste('Target ~',f )

# to fix error in neurons - requires matrix 
?matrix
nn_training <- training.data[train,]
nn_training <- matrix(as.numeric(unlist(nn_training)), nrow=nrow(nn_training))
colnames(nn_training) = names(training.data)

nn_test <- training.data[test,-138]
nn_test <- matrix(as.numeric(unlist(nn_test)), nrow=nrow(nn_test))

nn_model <- neuralnet(f, data = nn_training, hidden=c(20,10,10), linear.output = FALSE)
nn_predict <- compute(nn_model,nn_test)

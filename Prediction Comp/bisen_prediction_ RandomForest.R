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
library(randomForest)
library(caret)
library(nnet)

# Importing dataframe
training.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/training_data.csv")

test.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/test_df.csv")
OGtest.data <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/Prediction Comp/test_df.csv")

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
training.data$dependency = NULL
training.data$edjefa = NULL
training.data$edjefe = NULL

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
# so lets replace the missing values with median 
training.data$SQBmeaned[is.na(training.data$SQBmeaned)] = SQBmeaned_median

# check to make sure we do not have any missing values anymore
stopifnot(find_missing_rows(training.data)==0)



# Filling small gaps - for later
summary(training.data$meaneduc)
summary(training.data$SQBmeaned)

# Split data into training and test
  #train               <- sample(seq(nrow(training.data)),
  #                              floor(nrow(training.data) * 0.8))
  #train               <- sort(train)
  #test                <- which(!(seq(nrow(training.data)) %in% train))

# Running random forest
X_train = training.data[,-135]
Y_train = training.data[,135]
Y_train <- as.factor(Y_train)


#forest_model <- randomForest(Target~.,data= training.data[train,],  importance = TRUE)
forest_model <- randomForest(Y_train~.,data= X_train,  importance = TRUE)


# Set up test data
test.data$v2a1 = NULL
test.data$v18q1 = NULL
test.data$rez_esc = NULL
test.data$Id = NULL
test.data$idhogar = NULL
test.data$dependency = NULL
test.data$edjefa = NULL
test.data$edjefe = NULL

# check to make sure we do not have any missing values anymore
stopifnot(find_missing_rows(test.data)==0)


# Running random forest
X_test = test.data

forest_predict_test <- predict(forest_model,X_test)
 
# Export csv

newdf <- dplyr::select(test.data)
newdf$Prediction <- forest_predict_test
write.csv(newdf, file = "bisen_prediction.csv", row.names=FALSE)



##### For Reference - 3 Layer Neural Network #####
##### Random Forest Model Had Better Performance #####

#mlp_grid = expand.grid(layer1 = 100,
#                       layer2 = 100,
#                       layer3 = 100)
#
#mlp_fit = caret::train(as.factor(Y_train)~.,data=X_train, 
#                       method = "mlpML", 
#                       #preProc =  c('center', 'scale', 'knnImpute', 'pca'),
#                       preProc =  c('center', 'scale'),
#                       trControl = caret::trainControl(method = "cv", verboseIter = TRUE, returnData = FALSE),
#                       tuneGrid = mlp_grid)
#mlp_predict_train = predict(mlp_fit, newdata = X_train)
#mlp_predict_test = predict(mlp_fit, newdata = X_test)
#print(f1_score(mlp_predict_train, Y_train))
#print(f1_score(mlp_predict_test, Y_test))


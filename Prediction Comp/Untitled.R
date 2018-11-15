## API-222 
## Code by Arjun Bisen
## Competition
## Harvard Kennedy School

rm(list=ls())

# Common libraries
library(ISLR)
library(glmnet)
library(class)
library(pls)


###########################################################
# We are interested in predicting home sales prices (SalePrice). Drop the following columns: Id,
# MiscFeature, Fence, PoolQC, FireplaceQu, Alley. Also, make sure all numeric values are saved as
# type numeric rather than type integer (be careful in making this conversion to ensure values are preserved).

#load data frame
housing.data.test <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/alldatahousingadvancedregression/test.csv")
housing.data.training <- read.csv("/Users/arjunbisen/Documents/HKS/MachineLearning/alldatahousingadvancedregression/train.csv")
#explore data frame
head (housing.data.test, 10)
colnames(housing.data.test)

# drop columns: Id, MiscFeature, Fence, PoolQC, FireplaceQu, ALley
housing.test <- housing.data.test[-1, -7, -58, -73, -75]
housing.data.training <- housing.data.training[-1, -73, -75, -58, -7]
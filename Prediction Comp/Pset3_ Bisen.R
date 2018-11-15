## API 222
## PSet 3 : Machine Learning and Big Data
## Name : Arjun Bisen
## Harvard Kennedy School

rm(list=ls()) 

# Common libraries 

library(glmnet) 
library(class) 
library(pls)
library(fastDummies)
library(dplyr)
library(tree)
library(survival)

# Importing Dataframe
housing.data <- read.csv("Documents/Machine Learning/ProblemSet3_Data.csv")

# We are interested in predicting home sales prices (SalePrice). 
# Drop the following columns: Id, MiscFeature, Fence, PoolQC, FireplaceQu, Alley. 
# Also, make sure all numeric values are saved as type numeric rather than type integer
# (be careful in making this conversion to ensure values are preserved). 

colnames(housing.data)
housing_dirty <- housing.data[, -c(1, 74, 75, 73, 58, 7)]

# (b) How many observations have missing values? Drop those observations.
housing <- (na.omit(housing_dirty))
dim(housing_dirty)
dim(housing)
1460 - 1094


# (a) How many predictors are in the data set 
# (after you drop the variables according to the directions)?
dim(housing)
colnames(housing)
View(housing)

# (c) How many categorical string variables are in the data set? 
# These variables are not assigned numeric values that reflect an ordering. 
# Convert all categorical string variables to a complete set of indicator variables 
# (e.g. if the variable takes on four unique values, make four indicator variables).

sum(sapply(housing, class) == "string")

# Saving all values a numeric
for (i in ncol(housing):1) {
  print(i)
  if (is.factor(housing[,i])) {
    for (j in unique(housing[,i])) {
      print(j)
      new_col             <- paste(colnames(housing)[i], j, sep = "_")
      housing[,new_col] <- as.numeric(housing[,i] == j) 
    }
    housing       <- housing[,-i]                       
  } else if (typeof(housing[,i]) == "integer") {
    housing[,i]   <- as.numeric(as.character(housing[,i]))
  } 
}

# (d) How many of the variables (including the newly generated variables) have 
# variance <0.05? Drop all columns with variance <0.05. This step ensures your 
# subsequent code will run without erroring.

sum(sapply(housing, var)<0.05)
# drop columns
dummyhousing <- housing[,which(sapply(housing, var)>=0.05)]

#########################################
# Question 2:
# Run Principal Components Regression on the training data.
# (a) Would you say the model with 5 principal components shows a big 
# improvement in CV RMSE over the model with 0 principal components?
set.seed(222)
train               <- sample(seq(nrow(dummyhousing)),
                              floor(nrow(dummyhousing) * 0.8))
train               <- sort(train)
test                <- which(!(seq(nrow(dummyhousing)) %in% train))

pcr_fit           <- pcr(SalePrice~., data = dummyhousing[train,], 
                         scale = TRUE, validation = "CV")
summary(pcr_fit)

pcr_msep          <- MSEP(pcr_fit)
pcr_min_indx      <- which.min(pcr_msep$val[1,1,])
print(pcr_min_indx)

print(pcr_msep$val[1,1,])

validationplot(pcr_fit)


# Question 3: How many principal components yields the best CV RMSE? 
# (b) What is the corresponding CV RMSE?
# (c) What is the test RMSE?
pcr_pred          <- predict(pcr_fit, dummyhousing[test,],
                             ncomp = 99)

pcr_test_MSE      <- mean((pcr_pred - dummyhousing[test,"SalePrice"])^2)
print(pcr_test_MSE)

# convert this to RMSE

print(sqrt(pcr_test_MSE))

pls_fit           <- plsr(SalePrice~., data = dummyhousing[train,], 
                          scale = TRUE, validation = "CV")
summary(pls_fit)

## Which ncomp value had the lowest CV MSE?

pls_msep          <- MSEP(pls_fit)
pls_min_indx      <- which.min(pls_msep$val[1,1,])
print(pls_min_indx)

## Plot validation error as a function of # of components

validationplot(pls_fit)

# Run Partial Least Squares on the training data. (1 pt)
# (a) What number of components correspond to the lowest CV RMSE? 
# (b) What is that CV RMSE?
# (c) What is the test RMSE?
pls_rmsep         <- RMSEP(pls_fit)
print(pls_rmsep$val[1,1,as.numeric(pls_min_indx)])

## Predict test set SalePrice values

pls_pred          <- predict(pls_fit, dummyhousing[test,],
                             ncomp = (as.numeric(pls_min_indx) -1))

## Measure test MSE and RMSE

pls_test_MSE      <- mean((pls_pred - dummyhousing[test,"SalePrice"])^2)
print(pls_test_MSE)
print(sqrt(pls_test_MSE))

## Question 6
housingtree <- data.frame(dummyhousing)
q6tree <- tree(SalePrice ~.,data = housingtree[train,])
set.seed(222)
q6tree_class <- cv.tree(q6tree)

#plot(q6tree_class$size,q6tree_class$dev,type="b")
#plot(q6tree_class$k,q6tree_class$dev,type="b")

opt_indx <- which.min(q6tree_class$dev)
opt_size <- q6tree_class$size[opt_indx]

print(opt_size)
tree_predict <- predict(q6tree,housingtree[test,],ncomp=12)
sqrt(mean((tree_predict - dummyhousing[test,"SalePrice"])^2))

#Question 7
library(randomForest)
forest_model <- randomForest(SalePrice~.,data= housingtree[train,])
forest_predict <- predict(forest_model,housingtree[test,])

sqrt(mean((forest_predict - dummyhousing[test,"SalePrice"])^2))
print(forest_model)

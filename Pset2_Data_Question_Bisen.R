# Pset 2, Machine Learning and Big Data API 222
# Arjun Bisen's code
# Harvard Kennedy School

rm(list=ls())

install.packages("ISLR")
install.packages("glmnet")
install.packages("class")

library(ISLR)
library(glmnet)
library(class)
?Hitters
Hitters_Data <- Hitters

# Explore data
View(Hitters_Data)
summary(Hitters_Data)
dim(Hitters_Data)

# Clean data and number of observations with missing values
clean_data <- na.omit(Hitters_Data)
full <- dim (Hitters_Data)
clean <- dim (clean_data)
Missing_observation = (full) - (clean)
print(Missing_observation)
# there are 59 missing observations

# Which variables are categorical variables? How many classes do each of these categorical 
# variables have?
?Hitters
# League (2 classes), Division (2 classes), NewLeague (2 classes)
# Basic Scatterplot Matrix to see it visually
pairs(~AtBat + Hits + HmRun + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CWalks + League + Division + PutOuts + Assists + Errors + Salary + NewLeague, data = clean_data,
      main="Simple Scatterplot Matrix")

# Convert the categorical variables to indicator variables (also called “dummy" variables) 
# with the same variable names and run a linear regression. What is the adjusted R2?
clean_data$League  <- as.numeric(clean_data$League)
clean_data$Division  <- as.numeric(clean_data$Division)
clean_data$NewLeague  <- as.numeric(clean_data$NewLeague)
View(clean_data)

# For regression model
lm_HmRun <- lm(HmRun ~ AtBat + Hits + Runs + RBI + Walks + Years + CAtBat + CHits + CHmRun + CRuns + CRBI + CWalks + League + Division + PutOuts + Assists + Errors + Salary + NewLeague, clean_data)
print(lm_HmRun)
summary(lm_HmRun)

# Run ridge regression with cross-validation using the canned function cv.glmnet  from the package glmnet. 
set.seed(222)
ridge_clean_data <- cv.glmnet (x = as.matrix(clean_data[,-3]),
                            y = clean_data[,3],
                            alpha = 0)
print(ridge_clean_data$lambda)
print(ridge_clean_data$lambda.min)
#0.815

# What was the cross-validation eror?
which(ridge_clean_data$lambda == ridge_clean_data$lambda.min)
lowest_lambda <- which(ridge_clean_data$lambda == ridge_clean_data$lambda.min)
ridge_clean_data$cvm[lowest_lambda]
# 15.216

# What was the standard error of the mean cross-validation error for this value of ƛ?
ridge_clean_data$cvsd[lowest_lambda]
# 1.180

# What was the largest value of ƛ whose mean cross validation error was within one 
# standard deviation of the lowest cross-validation error? 
min_position <- which(ridge_clean_data$lambda == ridge_clean_data$lambda.min)
upper_bound <- ridge_clean_data$cvm[lowest_lambda] + ridge_clean_data$cvsd[min_position]

cv_poss <- which(ridge_clean_data$cvm <= upper_bound)
largest_lambda <- max(ridge_clean_data$lambda[cv_poss])
print(ridge_clean_data$lambda.1se)
# 1.182

# Using the same data, implement your own 5-fold cross-validation routine for KNN for k = 1… 100 (e.g. write 
#the cross-validation routine yourself rather than using a canned package). Include the snippet of code you 
#wrote here. It should not exceed 20 lines. Which k is best according to CV?
cross_validation_KNN <- function(data_x, data_y, k_seq, kfolds){
  fold_ids      <- rep(seq(kfolds), ceiling(nrow(data_x) / kfolds))
  fold_ids      <- fold_ids[1:nrow(data_x)]
  fold_ids      <- sample(fold_ids, length(fold_ids))
  CV_error_mtx  <- matrix(0,nrow = length(k_seq),ncol = kfolds)
  for (k in k_seq) { for (fold in 1:kfolds) {
    knn_fold_model <- knn(train = data_x[which(fold_ids != fold),],test = data_x[which(fold_ids == fold),],
                          cl = data_y[which(fold_ids != fold)],
                          k = k)
    CV_error_mtx[k, fold]  <- mean(knn_fold_model != data_y[which(fold_ids == fold)])}}  
  return(CV_error_mtx)}
set.seed(222)
knn_cv_error  <- cross_validation_KNN(data_x = clean_data[,-3],
                                      data_y = clean_data[,3],
                                      k_seq = seq(100),
                                      kfolds = 5)
View(knn_cv_error)
mean_cv_error <- rowMeans(knn_cv_error)
which.min(mean_cv_error)

# Plot mean cross-validation MSE as a function of k. Label the y-axis “Mean CV MSE" and the x-axis “k". 
plot(seq(100),
     mean_cv_error,
     type = "l",
     main = "Mean CV MSE as a Function of k",
     ylab = "Mean CV MSE",
     xlab = "k")


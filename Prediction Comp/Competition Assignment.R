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

## Example: Suppose your predictions were in a variable "preds" 
## and the true outcomes were in a variable "truth"
## This would print the F1 score:
## A larger F1 score is better than a smaller one.

  print(f1_score(preds, truth))
  
  
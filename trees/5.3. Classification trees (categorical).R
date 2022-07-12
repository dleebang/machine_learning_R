# Classification trees, or decision trees, are used in prediction problems where
# the outcome is categorical.
# 
# Decision trees form predictions by calculating which class is the most common
# among the training set observations within the partition, rather than taking
# the average in each partition (as in regression trees).
# 
# Two of the more popular metrics to choose the partitions are the GINI INDEX
# and ENTROPY (instead of the root mean square to minimize error in train set of
# regression tree).
# 
# p_hat_j,k = proportion of obs. in a partition j of class k
# 
#   Gini = sum_p_hat_j,k * (1 - p_hat_j,k)
#   
#   entropy(j) = -sum_p_hat_j,k * log(p_hat_j,k), with 0 * log(0) defined as 0
#   
# 
# PROS of classification trees: they are highly interpretable and easy to
# visualize. They can model human decision processes and don't require use
# of dummy predictors for categorical variables.
# 
# CONS: The approach via recursive partitioning can easily over-train and
# is therefore a bit harder to train, for example, than linear regression or kNN.
# Furthermore, in terms of accuracy, it is rarely best performing method since
# it is not very flexible and is highly unstable to changes in training data.
# 
#
## Code
library(tidyverse)
library(dslabs)
library(caret)
library(rpart)
data("mnist_27")

# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)

plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

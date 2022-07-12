# To mimic the valuation process, we randomly split our data into two:
# a TRAINING SET and a TEST SET - and act as if we don't know the ouctome of the
# test set. We develop algorithms using only the training set; the test
# set is used only for evaluation
# 
# We use the createDataPartition() function from the caret package to generate
# indexes for randomly splitting data
# 
# Note: contrary to what the documentation says, this course will use the
# argument p as the percentage of data that goes to testing.  The indexes
# made from createDataPartition() should be used to create the test set.
# Indexes shoud be created on the outcome and not a predictor
# 
# The simplest evaluation metric for categorical outcomes is overall accuracy:
# the proportion of cases that were correctly predicted in the test set

## Code
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height

# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE) 
# createDataPartition() arguments: 
#   y = vector; 
#   times = how many random samples of indexes to return
#   p = the proportion of the index represented
#   list = indexes to be returned as a list or not
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)

# explore data to see proportion of data
heights %>% group_by(sex) %>% 
  summarize(mean(height), sd(height))

# guess how many males are within two sd less from the male mean
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)

# use map_dbl() to map and apply a function to each element of a vector
# returning a vector of the same length as the input
accuracy <- map_dbl(cutoff, function(x){  #apply function to each element of cutoff
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

# plot to see which cutoff provides best accuracy estimate
data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# use best_cutoff to test accuracy on test_set
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))

mean(y_hat == test_set$sex)

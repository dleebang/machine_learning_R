# Baye's formula: 
# 
#   p(x) = Pr(Y=1|X=x) = f_X|Y=1(X) * Pr(Y=1) / f_X|Y=0(X) * Pr(Y=0) + f_X|Y=1(X) * Pr(Y=1)
#   
# with f_X|Y=1 and f_X|Y=0 representing the DISTRIBUTION FUNCTIONS of the predictor X
# for the two classes Y = 1 and Y = 0.
# 
# The Naive Bayes approach is similar to the logistic regression prediction
# mathematically (see in https://hastie.su.domains/Papers/ESLII.pdf)
# 
# 
## Code
# Generating train and test set
library(caret)
library(dslabs)
library(tidyverse)
data("heights")

y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations for each group
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence of females
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi

# Getting an actual rule with naive bayes formula
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))

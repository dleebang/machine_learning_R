# A tree is basically a flow chart of yes or no questions. The general idea of the
# methods we are describing is to define an algorithm that uses data to create these
# trees with predictions at the ends, referred to as nodes.
# 
# When the outcome is CONTINUOUS, we call the decision tree method a REGRESSION TREE.
# 
# Regression and decision trees operate by predicting and outcome variable Y by 
# PARTITIONING THE PREDICTORS.
# 
# The general idea is to build a decision tree and, at the end of each node, obtain a
# predictor y_hat. Mathematically, we are partitioning  the predictor space into
# J non overlapping regions, R_1, R_2, ..., R_J and then for any predictor
# x that falls within region R_J, estimate f(x) with the average of the training
# observations y_i for which the associated predictor x_i is also in R_j.
# 
# To pick j and its value s, we find the pair that minimizes the residual sum of 
# squares (RSS) 
# (see formula in https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+2T2021/block-v1:HarvardX+PH125.8x+2T2021+type@sequential+block@78cfd6af400d452c9aa9f8070403e55c/block-v1:HarvardX+PH125.8x+2T2021+type@vertical+block@56c9ab44dbe84c2ea4a566a87a7f8ab3):
# 
#       sum_R1(j,s)(y_i - y_hat_R1)^2  + sum_R2_(j,s)(y_i - y_hat_R2)^2
#       
# To fit the gression tree model, we use the rpart() function in the rpart package.
# 
# Two common parameters used for partition decision are the complexity parementer (cp)
# and the minimum number of observations required in a partition before partitioning
# (minsplit in the rpart package)
# 
# If you already have a tree and want to apply a higher cp value, we can use the
# prune() function. We call this pruning a tree because we are snipping off partitions
# that do not meet a cp criterion.
# 
# 
## Code
#load data
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 15, 2)), 
             data = olive)
ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic and segments in the graph to partition
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)

# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)

#make decision tree splitting the data
library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#see how the tree fits the data
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# change parameters = complexity parameters (cp) and minsplit
# cp = 0 will predict each point by itself
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))

# see how the decion tree now fits each point
polls_2008 %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# use cross validation to choose cp
library(caret)
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)

# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# see how the tree with the optimal cp nows fit the model
polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

# prune the tree 
pruned_fit <- prune(fit, cp = 0.01)

# see how the pruned tree fits the model
polls_2008 %>% 
  mutate(y_hat = predict(pruned_fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")

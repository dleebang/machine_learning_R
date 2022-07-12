# Linear regression can be considered a machine learning algorithm. Although
# it can be too rigid to be useful, it works rather well form some challenges.
# It also serves as a baseline approach: if you can't beat it with a more complex
# approach, you probably want to stick to linear regression
# 
# 

## Code
library(tidyverse)
library(HistData)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

library(caret)
y <- galton_heights$son
#create data partition for test and train sets
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

#define sets
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

#if we do not know the father's height, and we are just guessing the son's
#height and we get the average of son's height
avg <- mean(train_set$son)
avg

#the squared loss (Y_hat - Y)^2
mean((avg - test_set$son)^2)

#fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

#regression equation = beta_0 + beta_1 * x, where beta_0 is intercept and beta_1 slope 
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father

#squared loss of the fitted model
mean((y_hat - test_set$son)^2)

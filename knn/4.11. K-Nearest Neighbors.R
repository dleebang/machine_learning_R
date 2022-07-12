# K-nearest neighbors (kNN) estimates the conditional probabilities in a similar
# way to bin smoothing. However, kNN is easier to adapt to multiple dimensions.
# 
# Using kNN for any point (x_1, x_2) for which we want an estimate of p(x_1, x_2),
# we look for the K NEAREST POINTS to (x_1, x_2) and take an average of the 0s and
# 1s associated with these points. We refer to the set of points used to compute
# the average as the NEIGHBORHOOD. 
# 
# Larger values of k result in smoother estimates, while smaller values 
# of k result in more flexible and more wiggly estimates.
# 
# To implement the algorithm, we can use the knn3() function from the caret
# package. There are two ways to call this function:
# 
#   1. We need to specify a formula and a data frame. The formula looks like:
#   outcome ~ predictor1 + predictor2 + predictor3. The predict() function
#   for knn3 produces a probability for each class.
#   
#   2. We can also call the function with the first argument being the matrix
#   predictors and the second a vector of outcomes.
#   
#
## Code
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

#plot the predictors with outcome represented with color
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test) #predict condit. probs.
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2)) #predict classes based on a decision rule (0.5)
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1] #get accuracy

#fit knn model
#with formula
knn_fit <- knn3(y ~ ., data = mnist_27$train)

#defining matrices with x and y (useful with higher dimensions: more predictors)
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

#define parameter k = 5 (number of neighbors)
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

#predict classes based on knn model fit
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

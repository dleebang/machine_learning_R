# OVERTRAINING is the reason that we have higher accuracy in the train set
# compared to the test set. Overtraining is at its worst when we set k = 1.
# With k = 1, the estimate for each (x_1, x_2) in the training set is obtained
# with just the y corresponding to that point.
# 
# When we try a larger k, the k might be so large that it does not permit enough
# flexibility. We call this OVERSMOOTHING.
# 
# Note that if we use the test set to pick this k, we should not expect the 
# accompanying accuracy estimate to extrapolate to the real world. This is 
# because even here we broek a golden rule of machine learning: THE K IS
# SELECTED USING THE TEST SET. Cross validation also provides an estimate that
# takes this into account.
# 

## Code
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

#compare accuracies
#predict on train set
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
#predict on test set
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


#fit knn with k=1 to see OVERTRAINING
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)

#accuracy for train goes up to almost perfect predictions since it is using 
#single points for predictions
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

#accuracy for test goes down
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401 to see OVERFITTING
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
})

#plot the accuracy for each k for test and train datasets
bind_cols(accuracy, k = ks) %>% 
  gather(key = "set", value = "accuracy", train, test) %>% 
  ggplot(aes(k, accuracy, color = set)) +
  geom_point() +
  geom_line()

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)


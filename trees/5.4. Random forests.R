# Random forests are a very popular machine learning approach that addresses the
# shortcomings of decision trees. The goal is to improve prediction performance
# and reduce instability by AVERAGING MULTIPLE DECISION TREES (a forest of 
# trees constructed with randomness).
# 
# The general idea of random forests is to generate many predictors, each using
# regression or classification trees, and then forming a final prediction based
# on the average prediction of all these trees. To assure that the individual
# trees are not the same, we use the bootstrap to induce ramdomness.
# 
# A disadvantage of random forests is that we lose intepretability.
# 
# An approach that helps with interpretability is to examine VARIABLE IMPORTANCE.
# To define variable importance we COUNT HOW OFTEN A PREDICTOR IS USED IN THE
# INDIVIDUAL TREES. The caret package includes the function varImp that extracts
# variable importance from any model in which the calculation is implemented. 
# 
# 
## Code
install.packages("randomForest")
library(randomForest)
data("polls_2008")

#randomforest on polls data
fit <- randomForest(margin~., data = polls_2008) 
#see that around 200 trees the algorithm error doesn't change much
plot(fit)

#see how the randomforest model fit to the data (it is somehow smoothed since it is
#the average of many decision trees)
polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_line(aes(day, y_hat), col="red")

#randomforest on 2 and 7's data
train_rf <- randomForest(y ~ ., data=mnist_27$train)

#compute accuracy
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

# use cross validation to choose parameter
train_rf_2 <- train(y ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                    data = mnist_27$train)

#compute accuracy with optimized parameters from cross validation
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
           
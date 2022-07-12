# The train() function automatically uses cross-validation to decide among a
# few default values of a tuning parameter. 
# 
# THe getModelInfo() and modelLookup() functions can be used to learn more 
# about a model and the parameters that can be optimized.
# 
# We can use the tunegrid parameter in the train() function to select a grid
# of values to be compared (the input of the parameter must be given as a dataframe).
# 
# The trControl parameter and trainControl() function can be used to change the
# way cross-validation is performed. 
# 
# Note that NOT ALL PARAMETERS IN MACHINE LEARNING ALGORITHMS ARE TUNED. We
# use the train() function to only optmize parameters that are tunable. For example,
# Linear and logistic regression uses LSE (least squared error) to estimate
# the values of the model (such as intercept and slope).
# 
#
## Code
#get model info
getModelInfo("knn")
#get model parameters
getModelInfo("knn")$knn$parameters
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE) #plot the model parameters highlighting the optimized param.

#get best k from fitted model
train_knn$bestTune

#get outcomes with the best k
train_knn$finalModel

#compute accuracy (the predict function here will automatically use the 
#best performing model with the param. that maximized accuracy)
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]


#by default, cross validation method uses 25 bootstrap samples.
#For the knn method, the default is to try k = 5, 7, 9. 
#So we first change the k parameters using tuneGrid parameters.
#The grid of values must be supplied by a dataframe with the param. names 
#as specified in the modelLookup output.
set.seed(2008, sample.kind = "Rounding")
train_knn <- train(y ~ ., method = "knn", 
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

#access param. that maximized accuracy
train_knn$bestTune

#best performing model
train_knn$finalModel

#compute accuracy
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"),
                mnist_27$test$y)$overall["Accuracy"]


#If we want to change how we perform cross validation, we can use trainControl()
#Make the code a bit faster by using 10-fold cross validation ("cv")
control <- trainControl(method = "cv", number = 10, p = .9)

#incorporate control to the train function
train_knn_cv <- train(y ~ ., method = "knn", 
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)), #diff numbers for k
                      trControl = control)

ggplot(train_knn_cv, highlight = TRUE) #the accuracy astimates are more variable since we changed the number of bootstrap samples

#plot the accuracy standard deviation for each k parameter
names(train_knn$results)
train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

#plot conditional probabilities
plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

#the best fitting knn model approximates the true conditional probability
#but the boundary is somewhat wiggly
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])

#To improve this we could try loess function for local regression
install.packages("gam")
library("gam")

#see that both parameters that are tunable are span and degree
modelLookup("gamLoess")

#make a grid for different values for span and keep degree = 1
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

#incorporate tuning grid in the train() function using loess method
train_loess <- train(y ~ ., 
                     method = "gamLoess",
                     tuneGrid=grid,
                     data = mnist_27$train)

#plot the best span parameter that gives best accuracy estimate
ggplot(train_loess, highlight = TRUE)

#compute accuracy
confusionMatrix(data = predict(train_loess, mnist_27$test), 
                reference = mnist_27$test$y)$overall["Accuracy"]

#plot conditional probabilities of loess method
#see how smoother the boundary is relatively to true conditional probs.
plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])

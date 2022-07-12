library(caret)
library(tidyverse)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#Q7)
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test <- iris[test_index,] %>% droplevels(.)
train <- iris[-test_index,] %>% droplevels(.)

#Q8)
cutoff1 <- seq(min(train$Sepal.Length), max(train$Sepal.Length), 0.1)
accuracy1 <- map_dbl(cutoff1, function(x) {
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = c("versicolor", "virginica"))
  mean(y_hat == train$Species)
})
max(accuracy1)

cutoff2 <- seq(min(train$Sepal.Width), max(train$Sepal.Width), 0.1)
accuracy2 <- map_dbl(cutoff2, function(x) {
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = c("versicolor", "virginica"))
  mean(y_hat == train$Species)
})
max(accuracy2)

cutoff3 <- seq(min(train$Petal.Length), max(train$Petal.Length), 0.1)
accuracy3 <- map_dbl(cutoff3, function(x) {
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = c("versicolor", "virginica"))
  mean(y_hat == train$Species)
})
max(accuracy3)

cutoff4 <- seq(min(train$Petal.Width), max(train$Petal.Width), 0.1)
accuracy4 <- map_dbl(cutoff4, function(x) {
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = c("versicolor", "virginica"))
  mean(y_hat == train$Species)
})
max(accuracy4)

# other more effective approach
foo <- function(x){
  cutoffs <- seq(range(x)[1],range(x)[2],by=0.1) #range return a vector w/ min and max values
  sapply(cutoffs,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo) #argument 2 here indicates columns
sapply(predictions,max)	

#Q9)
foo(train[,3])
predictions <- foo(train[,3]) #make predictions for feature of q8 (petal.length)
rangedValues<-seq(range(train[,3])[1],range(train[,3])[2],by=0.1) #create a vector of seq. of unique values of petal.length
cutoffs <-rangedValues[which.max(predictions)] #find the best cutoff in the vector of unique values

y_pl <- ifelse(test$Petal.Length > cutoffs[1], "virginica", "versicolor")
mean(y_pl == test$Species)

#Q10)
foo <- function(x){
  cutoffs <- seq(range(x)[1],range(x)[2],by=0.1) #range return a vector w/ min and max values
  sapply(cutoffs,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo) #argument 2 here indicates columns
sapply(predictions,max)	

#Q11)
plot(iris,pch=21,bg=iris$Species)
#apply foo function created above to find predictions for each cutoff (each unique trait value)
predictions <- apply(train[,c("Petal.Length", "Petal.Width")], 2, foo)

#create a function to find best_cutoff
best_cutoff <- function(x, prediction){
  rangedvalues <- seq(range(x)[1], range(x)[2], by = 0.1)
  rangedvalues[which.max(prediction)]
}

#use multiple apply to find best-cutoff through columns
cutoffs <- mapply(best_cutoff, 
                  train[,c("Petal.Length", "Petal.Width")], 
                  predictions)

#predict with multiple conditions
y_hat_combined <- ifelse(test$Petal.Length > cutoffs[1] | 
                          test$Petal.Width > cutoffs[2],
                         "virginica", "versicolor")

mean(y_hat_combined == test$Species)

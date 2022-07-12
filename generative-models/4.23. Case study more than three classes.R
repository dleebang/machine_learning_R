# In this case study, we will go through a slightly more complex example: one with
# 3 classes instead of 2. Then we will fit QDA, LDA, and KNN models for prediction.
# 
# Generative models can be very powerful, but only when we are able to successfully
# approximate the joint distribution of predictors conditioned on each class.
# 
#
## Code
if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456, sample.kind="Rounding")

index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

#build train set
train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

#build test set
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

#plot distribution of diff classes
train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

#fit diff models
#Quadratic discriminant analysis
train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()

confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

#Linear discriminant analysis
train_lda <- train(y ~ ., method = "lda", data = train_set)

confusionMatrix(predict(train_lda, test_set), test_set$y)$table
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

#K-Nearest neighbors
train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)

confusionMatrix(predict(train_knn, test_set), test_set$y)$table
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

#the reason why qda and lda dont work well here is because of the lack of fit
#by seeing the plot you see the 1s clearly do not approximate a bivariate normal distribution
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")

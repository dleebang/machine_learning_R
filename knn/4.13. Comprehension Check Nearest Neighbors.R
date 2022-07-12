set.seed(1, sample.kind = "Rounding")
library(caret)
library(dslabs)
library(tidyverse)
data("heights")

y <- heights$sex

#Q1)
set.seed(1, sample.kind = "Rounding")
ind <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
# ALWAYS USE CREATEDATAPARTITION INDEX TO TEST_SET
test <- heights %>% slice(ind)
train <- heights %>% slice(-ind)



ks <- seq(1, 101, 3)
set.seed(1, sample.kind = "Rounding")
F_1 <- sapply(ks, function(k){
  knn_fit <- knn3(sex ~ height, data = train, k = k)
  y_hat <- predict(knn_fit, test, type = "class")
  F_meas(data = y_hat, reference = test$sex)
})

plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]


#Q2)
data("tissue_gene_expression")
tissue_gene_expression
str(tissue_gene_expression)

set.seed(1, sample.kind = "Rounding")
ind <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
test_x <- tissue_gene_expression$x %>% as.data.frame() %>% slice(ind)
test_y <- tissue_gene_expression$y %>% as.data.frame() %>% slice(ind)
test <- bind_cols(test_y, test_x) %>% rename("tissue" = 1)
colnames(test) <- make.names(colnames(test))

train_x <- tissue_gene_expression$x %>% as.data.frame() %>% slice(-ind)
train_y <- tissue_gene_expression$y %>% as.data.frame() %>% slice(-ind)
train <- bind_cols(train_y, train_x) %>% rename("tissue" = 1)
colnames(train) <- make.names(colnames(train))

set.seed(1, sample.kind = "Rounding")
k <- seq(1, 11, 2)
accuracy <- sapply(k, function(i){
  knn_fit <- knn3(tissue ~ ., data = train, k = i)
  y_hat <- predict(knn_fit, test, type = "class")
  confusionMatrix(y_hat, reference = test$tissue)$overall["Accuracy"]
})

names(accuracy) <- k
accuracy

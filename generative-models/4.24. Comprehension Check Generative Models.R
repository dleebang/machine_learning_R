library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 

ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Q1)
#using formula (e.g. y ~ x will require a data to be specified)
#using not as a formula will require a matrix as x = predictors
train_lda <- train(x, y, method = "lda")


#Q2)
train_lda$finalModel$means
plot(train_lda$finalModel$means[1,], train_lda$finalModel$means[2,], 
     xlab = "hippocampus",
     ylab = "cerebellum")


#Q3)
set.seed(1993, sample.kind="Rounding")

ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_qda <- train(x, y, method = "qda")


#Q4)
train_qda$finalModel$means

t(train_qda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#Q5)
train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda$finalModel$means

t(train_lda$finalModel$means) %>% data.frame() %>% 
  mutate(predictor = rownames(.)) %>% 
  ggplot(aes(cerebellum, hippocampus, label = predictor)) + 
  geom_point() +
  geom_text() +
  geom_abline()

#or 
d <- apply(train_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)


#Q6)
set.seed(1993, sample.kind="Rounding") 
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

fit_lda <- train(x, y, method = "lda")
fit_lda$results["Accuracy"]

#we see that the results are slightly worse when looking at all of the tissue types
#instead of only selected ones. We can see at the confusionMatrix function to
#learn more about what type of errors we are making:
confusionMatrix(fit_lda)

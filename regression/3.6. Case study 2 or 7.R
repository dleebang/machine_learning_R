# In the digits example we want to identify whether a number is 2 or 7
# based on the quantity of pixels in the quadrants x_1 and x_2 of an image
# 
# In this case study, we apply logistic regression to classify whether
# a digit is 2 or 7. We are interested in estimating a conditional probability
# that depends on two variables:
# 
#   g{p(x_1, x_2)} = g{Pr(Y = 1 | X1 = x_1, X2 = x_2)} = beta_0 + beta_1*x_1 + beta_2*x_2
#   
# Through this case, we know that logistic regression forces our estimates to be
# a PLANE and our boundary of conditional probabilities x_1 and x_2 to be a LINE. 
# This implies that a logistic regression
# approach has no chance of capturing the NON-LINEAR nature of the true p(x_1, x_2).
# Therefore, we need other more flexible methods that permit other shapes.
# 

## Code
library(dslabs)
library(tidyverse)
library(caret)

mnist <- read_mnist()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_1), which.max(mnist_27$train$x_1))]

titles <- c("smallest","largest")

tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})

tmp <- Reduce(rbind, tmp)

#plot digits grids of 2
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

#plot distribution of conditional probabilities of x_1 and x_2
data("mnist_27")
mnist_27$train %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

is <- mnist_27$index_train[c(which.min(mnist_27$train$x_2), which.max(mnist_27$train$x_2))]
titles <- c("smallest","largest")

tmp <- lapply(1:2, function(i){
  expand.grid(Row=1:28, Column=1:28) %>%
    mutate(label=titles[i],
           value = mnist$train$images[is[i],])
})

tmp <- Reduce(rbind, tmp)

#plot digit grids for 7
tmp %>% ggplot(aes(Row, Column, fill=value)) +
  geom_raster() +
  scale_y_reverse() +
  scale_fill_gradient(low="white", high="black") +
  facet_grid(.~label) +
  geom_vline(xintercept = 14.5) +
  geom_hline(yintercept = 14.5)

#use glm to fit logistic regression model on the train set
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
#predict the conditional probabilities on test_set based on LSE of glm
p_hat_glm <- predict(fit_glm, mnist_27$test)
#predict the outcome based on conditional probs. of p_hat_glm
#the decision rule here is p(x, y) = 0.5
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
#build confusion matrix to evaluate metrics of the algorithm
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall["Accuracy"]

#plot conditional probabilities
mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

#ameliorate colors and plot boundary line
#note the curved shape of the line
mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

#compare predict conditional probabilities with true probabilities
#note that the glm model will always provide a line for the estimates
#which does not capture the non_linear nature of true probabilities
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)

#plot with geom_raster
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 

#plot points
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)

mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

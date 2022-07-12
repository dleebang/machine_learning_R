library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

#Q1)
set.seed(1, sample.kind="Rounding")
RMSE <- replicate(n, {
  index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-index)
  test_set <- dat %>% slice(index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)

#Q2)
n <- c(100, 500, 1000, 5000, 10000)

set.seed(1, sample.kind="Rounding")
dataset <- function(n) {
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(100, {
    index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-index)
    test_set <- dat %>% slice(index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(avg = mean(RMSE), sd = sd(RMSE))
}

results <- sapply(n, dataset) 

#Q3)
"On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases."

#Q4)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
RMSE <- replicate(n, {
  index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-index)
  test_set <- dat %>% slice(index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)

#Q5)
"When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y."

#Q6)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding")
index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-index)
test_set <- dat %>% slice(index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x1 <- sqrt(mean((y_hat - test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x2 <- sqrt(mean((y_hat - test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x12 <- sqrt(mean((y_hat - test_set$y)^2))

#Q7)
#0.3070962

#Q8)
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

cor(dat)

set.seed(1, sample.kind="Rounding")
index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-index)
test_set <- dat %>% slice(index)

fit <- lm(y ~ x_1, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x1 <- sqrt(mean((y_hat - test_set$y)^2))

fit <- lm(y ~ x_2, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x2 <- sqrt(mean((y_hat - test_set$y)^2))

fit <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(fit, test_set)
rmse_x12 <- sqrt(mean((y_hat - test_set$y)^2))

"Adding extra predictors can improve RMSE substantially, but not when the added predictors are highly correlated with other predictors."

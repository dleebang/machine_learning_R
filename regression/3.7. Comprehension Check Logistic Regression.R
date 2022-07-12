library(tidyverse)
library(caret)
set.seed(2, sample.kind="Rounding")

make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% ggplot(aes(x, color = y)) + geom_density()




####
set.seed(1, sample.kind="Rounding")
mu_seq <- seq(0, 3, len = 25)


res <- sapply(mu_seq, function(i) {
  dat <- make_data(mu_1 = i)
  fit_glm <- dat$train %>% glm(y ~ x, data =., family = "binomial")
  p_hat <- predict(fit_glm, dat$test)
  y_hat_glm <- ifelse(p_hat > 0.5, 1, 0) %>% factor(levels = c(0,1))
  mean(y_hat_glm == dat$test$y)
})

qplot(mu_seq, res)






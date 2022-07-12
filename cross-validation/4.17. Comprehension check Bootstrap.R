library(dslabs)
library(caret)
data(mnist_27)

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

#Q1)
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#Q2)
sum(sapply(indexes, function(x) sum(x == 3)))

#Q3)
y <- rnorm(100, 0, 1)
quantile(y, 0.75)
set.seed(1, sample.kind="Rounding")
B <- 10000
M <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(M)
sd(M)

#Q4)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
ind <- createResample(y, 10)

quantiles <- sapply(ind, function(x) quantile(y[x], 0.75))
median(quantiles)
sd(quantiles)

#Q5)
set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding")
ind <- createResample(y, 10000)

quantiles <- sapply(ind, function(x) quantile(y[x], 0.75))
median(quantiles)
sd(quantiles)

#Q6)
#True: The bootstrap is particularly useful in situations when we do not have access to the distribution or it is unknown.
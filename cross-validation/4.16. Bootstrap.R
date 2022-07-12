# When we dont have access to the entire population, we can use bootstrap to
# estimate the population median m.
# 
# The bootstrap permits us to approximate a Monte Carlo simulation without
# access to the entire distribution. The general idea is relatively simple. 
# We act as if the observed sample is the population. We then sample datasets
# (with replacement) of the same sample size as the original dataset. Then 
# we compute the summary statistic, in this case the MEDIAN, on this 
# bootstrap sample.
# 
# Note that we can use ideas similar to those used in the bootstrap in 
# cross validation: instead of dividing data into equal partitions, 
# we simply bootstrap many times.
# 

## Code
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind="Rounding")
N <- 250
X <- sample(income, N)
M<- median(X)
M

library(gridExtra)
B <- 10^5
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})

p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

B <- 10^5
M_star <- replicate(B, {
  X_star <- sample(X, N, replace = TRUE)
  median(X_star)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95)) #monte carlo conf. intervals
quantile(M_star, c(0.05, 0.95)) #bootstrap conf. intervals

#calculating conf. intervals with CLT (wrong)
median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

#if we know the dist. is normal, than we can use bootstrap to estimate
#the mean and standard error and form a conf. interval
mean(M) + 1.96 * sd(M) * c(-1,1)
mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

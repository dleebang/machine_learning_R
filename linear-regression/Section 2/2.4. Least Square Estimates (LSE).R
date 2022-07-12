# For regression, we aim to find the coefficient values that minimize 
# the distance of the fitted model to the data
# 
# Residual sum of squares (RSS) measures the distance between the true value
# and the predicted value given by the regression line. The values that 
# minimize the RSS are called the least squares estimates (LSE).
# 
# We can use partial derivatives to get the values for beta0 and beta1 (intercept
# and slope, respectively) in Galton's data. The values for betas are the estimates.
# 
# 
# Least square equation in: https://rafalab.github.io/dsbook/linear-models.html#least-squares-estimates-lse
# 

## Code:
# compute RSS for any pair of beta0 and beta1 in Galton's data
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# a function to compute rss for any pair of values beta0 and beta1
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# create a sequential vector of values for beta1 and fix beta0 at 25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))

# plot RSS as a function of beta1 when beta0=25
results %>% ggplot(aes(beta1, rss)) + 
  geom_line() + 
  geom_line(aes(beta1, rss))

# There's a clear minimum at 0.65. However, this minimum for beta1 is for
# when beta0 = 25, a value arbitrarily picked. We don't know if (25, 0.65) is
# the pair that minimizes the equation across all possible pairs. 
library(dslabs)
library(tidyverse)
data("heights")

#Q6)
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

#Q7)
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, breaks = quantile(height, ps), include.lowest = TRUE)) %>% 
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#Q8)
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat) #plot to see bivariate normal distribution
head(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

## Explanation:
## dat contains 10000 random pairs of values from a bivariate normal distribution
## with expected means 69 for both x and y, sigma = 9, and correlation = 0.5. 
## Then make a plot of CONDITIONAL EXPECTATIONS, given x and y as normal values.
## This way, define cuts (groups) based on quantiles (from normal dist.) for
## each value of x, so each value will be contained within the range defined
## by the cut of quantiles. Then, you re calculating Y = y from the pairs
## with x within the range of each group. 
## Each conditional expectation will then be estimated by the mean of X, and
## their corresponding Y's conditioned to X within the range of each group.
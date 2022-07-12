# Local weighted regression (loess) is used because of limitations of the bin
# smoothing approach. For bin smoothing we need small windows for the approximately
# constant assumptions to hold which may lead to imprecise estimates of f(x).
# Loess permits us to consider larger window sizes for regression
# 
# The Taylor's theorem tells us that if you look closely at any smooth
# function f(x), it will look like a line. 
# 
# Instead of assuming the function is approx. constant in a window, we assume
# the function is locally linear. We can consider larger window sizes with
# the linear assumption than with a constant. 
#
# There are three important differences between loess and bin smooth approach:
#   
#   1) Rather than keeping the bin size the same, loess keeps the number of 
#   points used in the local fit the same. This number is controlled via
#   argument span, which expects a proportion. For example, if N is the
#   number of data points and span = 0.5, then for given x, loess will use the
#   0.5 * N closest to points to x for the fit.
#   
#   2) When fittint a line locally, loess uses a weighted approach. Basically,
#   instead of using least squares we minimize a weighted version (see formula
#   in https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess)
#   and instead of the Gaussian kernel, loess uses a function called the
#   Tukey tri-weight (see formulas and kernels in the link above).
#   
#   3) Loess has the option of fitting the local model robustly. 
#   An iterative algorithm is implemented in which, after fitting
#   a model in one iteration, outliers are detected and down-weighted for the
#   next iteration. This is used with the argument familiy = "symmetric".
#
# 
## Code
library(dslabs)
library(tidyverse)
data("polls_2008")
polls_2008

#define total days
total_days <- diff(range(polls_2008$day))

#define the span proportion of window size
span <- 21/total_days

#fit loess model
#degree 1 fits a line for each window 
fit <- loess(margin ~ day, degree=1, span = span, data=polls_2008)

#plot the fitted model
#the final result is a smoother fit than the bin smooth since we use
#larger sample sizes to estimate local parameters
polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day, smooth), color="red")


#see how model fits with different spans in https://rafalab.github.io/dsbook/smoothing.html#local-weighted-regression-loess
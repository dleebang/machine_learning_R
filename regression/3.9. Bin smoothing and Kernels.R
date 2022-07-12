# The general idea of smoothing is to group data points into strata in which
# the value of f(x) can be assumed to be constant. We can make this assumption
# because we think f(x) changes slowly and, as a result, f(x) is almost constant
# in small windows of time.
# 
# This assumption implies that a good estimate for f(x) is the average of the Y_i
# values in the window. The estimate is:
# 
#   f_hat(x_0) = 1/N_0 * sum(Yi)
#   #see formula in https://rafalab.github.io/dsbook/smoothing.html
#   
# In smoothing, we call the size of the interval |x - x_0| satisfying this
# particular condition the WINDOW SIZE, BANDWIDTH or SPAN.
# 

## Code
library(dslabs)
library(tidyverse)
data("polls_2008")
polls_2008

# bin smoothers
span <- 7 
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="box", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

# note that the line resulting from bin smoothing is quite wiggly. 
# this happens because each point to compute average has the same weight
# 
# we can attenuate this by weighting center points than far away points in a
# given interval, with the two points at the edges receveing very little weight
# 
# we can think of bin smother approach as a weighted average:
# 
#     see formula in https://rafalab.github.io/dsbook/smoothing.html
#     
# we can use the kernel = normal to use normal density to assign weights
# to points
span <- 7
fit <- with(polls_2008, ksmooth(day, margin,  x.points = day, kernel="normal", bandwidth = span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

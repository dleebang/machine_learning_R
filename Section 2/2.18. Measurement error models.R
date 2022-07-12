# All our linear regression examples until now have been applied to two or more
# random variables. We assume the pairs are bivariate normal and use this to
# motivate a linear model.
# 
# Another use for linear regression is with MEASUREMENT ERROR MODELS, where
# it is common to have a non-random covariate (such as time). Randomness is
# introduced from measurement error rather than sampling or natural variability.

## Code
#use rfalling_object from dslabs
library(dslabs)
library(broom)
falling_object <- rfalling_object()

#plot the trajectory of the ball
falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")

#Use lm() to estimate coefficients
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)

#check if estimated parabola fits the data
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

#summary statistics of the regression
tidy(fit, conf.int = TRUE)

#initial height falls within the confidence intervals of intercept
#initial time of 0 falls within the conf. intervals of time with p value
#lower than .05 (not reject the null hypothesis that starting velocity is 0)
#the acceleration constant is in the conf. intervals of time_sq for
#negative 2 times beta2

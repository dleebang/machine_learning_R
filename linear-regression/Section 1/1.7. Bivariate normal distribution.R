# When a pair of random variables are approximated by the bivariate normal
# distribution, scatterplots look like ovals, rangin from thinner (high correl.)
# to circle-shaped (no correl.)
# 
# When two variables follow a bivariate normal distribution: if X is a normally
# distributed random variable, Y is also a normally distributed random variable,
# and the conditional distribution of Y for any X = x is approximately normal,
# then the pair is approx. bivariate normal.
# 
# When two variables follow a bivariate normal distribution, computing the
# regression line is equivalent to computing conditional expectations.
# 
# We can obtain a much more stable estimate of the conditional expectation
# by finding the regression line and using it to make predictions.
# 

## Code:
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)
# Because LSE are derived from samples, they are random variables as well.
# 
# beta0 and beta1 appear to be normally distributed because the central limit
# theorem applies here (the larger the N, the more normally distributed the 
# estimates will be)
# 
# The t-statistic is not based on the CLT, but rather depends on the assumption 
# that epslon follows a normal distribution. Under this assumption, mathematical
# theory tells us that the LSE divided by their standard error:
# 
#       beta0 / SE(beta0) and beta1 / SE(beta1)
# 
# follow a t distribution with N - p degrees of freedom, in which p is the
# number of parameters in the model (in this case, 2: beta0 and beta1)
# 

## Code
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# the reason why the plots look normal is because the central limit theorem
# applies here as well: for large enough N, the LSEs will be approximately
# normal with expected values beta0 and beta1, respecively.

# the standard errors are included in the summary provided by the lm function

# summary statistics of the random sampling
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

# the standard error estimates reported by the summary are close to the 
# standard errors from the simulation
lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

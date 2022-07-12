# The correlation we compute and use as a summary is a random variable
# Because we make N independent draws from the entire popoulation
# 
# When interpreting correlations, it is important to remember that 
# correlations derived from samples are estimates containing uncertainty
# 
# Because the sample correlation is an average of independent draws, the 
# central limit theorem applies. 
# 

## Code
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 500
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
# slope is represented by the derivation of standard deviation of the sample correlation
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2))) 
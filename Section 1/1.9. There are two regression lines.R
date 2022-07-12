# There are two different regression lines depending on whether we are taking the
# expectation of Y given X or taking the expectation of X given Y
# 
## Code
# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x #slope
b_1 <- mu_y - m_1*mu_x #intercept

# the regression line of the conditional expectation Y = son, X = father
# E(Y | X = x) (Y conditioned on X, son's height condition on father's height)
# is E(Y | X = x) = 37.3 + 0.46x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y #slope
b_2 <- mu_x - m_2*mu_y #intercept

# the regression line of the conditional expectation X = father, Y = son
# E(X | Y = y) (X conditioned on Y, father's height condition on son's height)
# is E(X | Y = y) = 40.9 + 0.41y


# We see regression to the average: the prediction for the father is closer
# to the father average than the son heights y is to the son average.

# plot the two regression lines, with the blue for predicting son heights with
# father heights, and red for predicting father heights with son heights
galton_heights %>% 
  ggplot(aes(father, son)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(intercept = b_1, slope = m_1, col = "blue") +
  geom_abline(intercept = -b_2/m_2, slope = 1/m_2, col = "red") 
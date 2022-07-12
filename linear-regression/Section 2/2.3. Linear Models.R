# We have described that if data follows a bivariate normal distribution,
# then the conditional expectations follow the regression line. The fact
# that the conditional expectation is a line is not an extra assumption but rather
# a DERIVED RESULT. However, in practice it is common explicitly write down
# a model that describes the relationship between two or more variables using
# a linear model.
# 
# We note that linear does not refer to lines, but rather to the fact that
# the conditional expectation (how one variable changes given one condition) is
# a LINEAR COMBINATION OF KNWON QUANTITIES MULTIPLIED BY A CONSTANT
# and add them together, for ex:
#  x an y are variables
#  in the linear model, multiply by a and b: xa + yb
# 
# In Galton's model, we assume Y (son's height) is a linear combination of a 
# constant and X (father's height) plus random noise (epslon = error). We
# further assume that epslon_i are independent from each other, have expected
# value 0 and the standard deviation sigma, which does not depend on i.
# 
# Note that if we further assume that epslon is normally distributed, then
# the model is exactly the same one we derived earlier by assuming bivariate
# normal data. The extra term epslon in the linear model explains variability.
# 
# We can subtract the mean from X to make beta_0 more interpretable. This
# causes the intercept value to be the predicted value of the conditional variable mean
# When calling the lm() function:
# THE VARIABLE WE WANT TO PREDICT IS PUT TO THE LEFT OF THE TILDE SYMBOL
# THE VARIABLES WE USE TO PREDICT IS PUT TO THE RIGHT OF THE TILDE SYMBOL
# The intercept is added automatically.
# 
# Least square estimates are random variables. 
# 
# You can create a fit object and use summary() to extract more information
# of the modeled fit
# 

## Code
# fit regression line to predict son's height from father's height
fit <- lm(son ~ father, data = galton_heights)
fit

# summary statistics
summary(fit)

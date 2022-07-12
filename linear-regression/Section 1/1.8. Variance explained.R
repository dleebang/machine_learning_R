# Conditioning on a random variable X can help to reduce variance of response 
# variable Y
# 
# The standard deviation of the conditional distribution is:
#     SD ( Y | X = x ) = sigma_y * sqrt(1 - rho^2)
# which is smaller than the standard deviation without conditioning sigma_y
# 
# Because variance is the standard deviation squared, the variance of the
# conditional distribution is:
#     sigma_y^2 * (1 - rho^2) 
#     
# In the statement "X explains such and such percent of the variability", the
# percent value refers to the variance. The variance decreases by rho^2 percent.
# 
# However, the "variance explained" statement only makes sense when the data
# is approximated by a bivariate normal distribution.
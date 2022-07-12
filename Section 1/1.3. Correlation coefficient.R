# The correlation coefficient is defined for a list of pairs (x1, y1), ... 
# (xn, yn) as the product of the standardized (z-scaled) values 
#   (xi - mu_x / sigma_x) * (yi - mu_y / sigma_y)
#   
# The correlation coefficient essentially conveys how two variables move
# together
# 
# The correlation coefficient is always between -1 and 1. Coefficients
# closer to values of 1, either negative or positive, show a thinner 
# distribution cloud of points, while values closer to 0 tend to 
# thicker the distribution. Correlation of 0 shows a round cloud of points
# 

## Code
rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

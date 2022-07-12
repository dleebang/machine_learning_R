# Smoothing is a powerful technique used all across data analysis. It is designed
# to detect trends in the presence of noisy data in cases in which the shape
# of the trend is unknown. 
# 
# The concepts behind smoothing techniques are extremely useful in machine learning
# because conditional expectations/probabilities can be though of as TRENDS
# of unknown shapes that we need to estimate in the presence of uncertainty.
# 

## Code
library(dslabs)
library(tidyverse)
data("polls_2008")
polls_2008
qplot(day, margin, data = polls_2008)

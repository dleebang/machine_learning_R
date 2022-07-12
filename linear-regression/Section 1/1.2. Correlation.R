# Galton tried to predict sons' heights based on father's heights
# 
# The mean and standard erros are insufficient for describing an important
# characteristic of the data: the trend that the taller the father, the taller
# the son
# 
# The correlation coefficient is an informative summary of how two variables
# move together that can be used to predict one variable using the other
# 

## Code:
# create the dataset
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
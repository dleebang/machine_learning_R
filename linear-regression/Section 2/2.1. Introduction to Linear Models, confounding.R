# Remember that association is not causation
# 
# Although it may appear that Bases on Balls (BB) cause runs, it is actually
# the Home runs (HR) that cause most of these runs. We say that BB are 
# confounded with HR.
# 
# Regression can help us account for confounding.
# 

## Code
# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))


## Alternatively you can write a function to get the slope and use it
## instead of lm(). lm() function gives the two coefficients intercept
## and slope:
get_slope <- function(x, y) cor(x,y) * sd(y) / sd(x)

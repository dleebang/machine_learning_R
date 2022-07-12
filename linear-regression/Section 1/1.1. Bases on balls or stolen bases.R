# See context of moneyball and baseball rules on edx website and book
# https://rafalab.github.io/dsbook/linear-models.html#case-study-moneyball
# 
# The visualization of choice when exploring the relationship btw two variables
# like home runs and runs is a scatterplot
# 

## Code: 
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

#scatterplot of the relationship btw HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#scatterplot of the relationship btw stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#scatterplot of the relationship btw bases and balls and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#scatterplot of the relationship btw runs per game and at bats
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

#scatterplot of the relationship btw win rate and fielding errors
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, errors_per_game = E/G) %>%
  ggplot(aes(win_rate, errors_per_game)) + 
  geom_point(alpha = 0.5)

#scatterplot of the relationship btw triples and doubles
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples = X3B/G, doubles = X2B/G) %>%
  ggplot(aes(triples, doubles)) + 
  geom_point(alpha = 0.5)


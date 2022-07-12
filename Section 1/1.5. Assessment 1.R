library(Lahman)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% 
  pull(r)


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(win_rate = W/G, errors_per_game = E/G) %>% 
  summarize(r = cor(win_rate, errors_per_game)) %>% 
  pull(r)


Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(triples = X3B/G, doubles = X2B/G) %>% 
  summarize(r = cor(triples, doubles)) %>% 
  pull(r)
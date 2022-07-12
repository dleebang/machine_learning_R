BB <- coefs$estimate[2]
single <- coefs$estimate[3]
double <- coefs$estimate[4]
triple <- coefs$estimate[5]
HR <- coefs$estimate[6]
intercept <- coefs$estimate[1]
  
teamA <- intercept + (2*BB) + (4*single) + double + HR
teamB <- intercept + BB + (6*single) + (2*double)


Teams %>% filter(yearID == 1971) %>% 
  mutate(BB_per_game = BB / G,
         HR_per_game = HR / G,
         R = R / G) %>% 
  do(tidy(lm(R ~ BB_per_game + HR_per_game, data = .)))



Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .), conf.int=T)) %>% 
  filter(term == "BB") %>% 
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")


q10 <- Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .), conf.int=T)) %>% 
  filter(term == "BB")


  tidy(lm(estimate ~ yearID, data = q10), conf.int=T)


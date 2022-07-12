library(Lahman)

head(Teams)
data <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G,
         BB_per_game = BB / G,
         R_per_game = R / G)

data %>% lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>% 
  .$coef
  

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

### you can use predict with confidence intervals to get values of min an
### max limits of the confidence intervals
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))
###

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))



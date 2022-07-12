set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#Q7)
fit <- lm(mother ~ daughter, data = female_heights)


#Q8)
predictions <- predict(fit)
data <- as_tibble(predictions) %>% bind_cols(mother = female_heights$mother)

#Q9)
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))

bat_01 %>% filter(mean_singles > 0.2) %>% nrow()
bat_01 %>% filter(mean_bb > 0.2) %>% nrow()

#Q10)
combined_bats <- inner_join(bat_02, bat_01)
head(combined_bats)
cor(combined_bats$singles, combined_bats$mean_singles)
cor(combined_bats$bb, combined_bats$mean_bb)

#Q11)
combined_bats %>% ggplot(aes(mean_singles, singles)) +
  geom_point()

combined_bats %>% ggplot(aes(mean_bb, bb)) +
  geom_point()

#Q12)
combined_bats %>% lm(singles ~ mean_singles, data = .)
combined_bats %>% lm(bb ~ mean_bb, data = .)

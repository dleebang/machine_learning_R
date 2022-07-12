set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
mu_daut <- mean(female_heights$daughter)
sd_daut <- sd(female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

slope <- r * (sd_daut / sd_mother) # m in equation of regression line
intercept <-  mu_daut - slope*mu_mother # b in equation of regression line

r^2 * 100 # variance explained is coefficient correlation (rho) squared times 100

intercept + slope * 60


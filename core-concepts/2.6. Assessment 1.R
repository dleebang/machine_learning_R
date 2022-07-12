library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

dat

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#Q1)
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#Q2)
type <- levels(factor(dat$type))
y_hat <- ifelse(dat$type == "inclass", "Female", "Male") %>% 
  factor(levels = levels(y))
mean(y_hat == y)

#Q3)
table(y_hat, y)

#Q4)
sensitivity(y_hat, y)

#Q5)
specificity(y_hat, y)

#Q7)
mean(y == "Female")

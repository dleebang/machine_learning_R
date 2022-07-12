# The predicted value is often denoted as Y_hat, wich is a random variable.
# Pluggin in the estimates into the regression model:
# 
#     Y_hat = beta0 + beta1 * x
#   
# 
# Mathematical theory tells us what the standard error of the predicted value
# is. If we assume the errors are normal or have a large enough sample size to use
# the Central Limit Theorem, we can construct confidence intervals of the prediction
# as well.
# 
# ggplot geom_smooth() allows setting method equals to lm
# 
# The predict() function in R can guve us predictions directly.
# 

## Code
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()


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

# Logistic regression is an extension of linear regression that assures that 
# the estimated conditional probability Pr(Y = 1 | X = x) is between 0 and 1.
# 
# This approache makes use of the logistic transformation:
# 
#   g(p) = log(p/(1-p))
#   
# With logistic regression, we model the conditional probability directly with:
# 
#   g{Pr(Y = 1 | X = x)} = beta_0 + beta_1 * x
#   
# Note that with this model, we can no longer use least squares. Instead we
# compute the MAXIMUM LIKELIHOOD ESTIMATE (MLE)
# 
# We can fit the logistic regression model with the function glm(), which stands
# for generalized linear models. If we want to compute the conditional probabilities,
# we want type = "response" since the default is to return the logistic transformed
# values.
# 
# 
## Code
#objects and datasets defined in the previous script
#plot linear regression line
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

#predictions can show negative values
range(p_hat)

#but we want conditional probabilities that's between 0 and 1
#so we fit a LOGISTIC REGRESSION MODEL
glm_fit <- train_set %>% 
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data=., family = "binomial") #family argument specify the logistic regression model because glm is more general

#make predictions based on the new model
#the type = response argument specify that we want the function to return
#conditional probabilities instead of logistic transformed values
p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) 

#compute logistic distribution function (plogis()) taking into account
#least square estimates from glm_fit. p_hat is the estimate/prediction for each value
#of x given the lse of the glm
logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))

#plot the logistic curve
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

#now predict the sex given the conditional probabilities
#if p_hat_logit (compute probs from glm model) higher than 0.5, return female
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor

confusionMatrix(y_hat_logit, test_set$sex)$overall[["Accuracy"]]

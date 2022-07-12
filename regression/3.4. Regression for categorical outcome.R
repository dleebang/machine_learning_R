# The regression approach can be extended to categorical data. For example,
# we can try regression to estimate the conditional probability:
# 
#   p(x) = Pr(Y = 1 | X = x) = beta_0 + beta_1 * x
#   
# Once we have estimates beta_0 and beta_1, we can obtain an actual prediction
# p(x), Then we can define a specific decision rule to form a prediction.
# 
#
## Code
library(dslabs)
data("heights")
y <- heights$height

set.seed(2, sample.kind = "Rounding")

#create train and test datasets
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#round to the nearest inch and predict the sex of a student who is 66 inches
#tall
train_set %>% 
  filter(round(height)==66) %>%
  summarize(y_hat = mean(sex=="Female"))

#plot proportion of females for each height group, excluding those with
#few data points
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

#calculate least square estimates (beta_0 and beta_1) for binary data
#transform Female or Male vector into 0s and 1s
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% 
  lm(y ~ height, data = .)

#make predictions
p_hat <- predict(lm_fit, test_set)

#decision rule of conditional probability of 0.5
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()

#build confusion matrix and extract overall accuracy
confusionMatrix(y_hat, test_set$sex)$overall["Accuracy"]




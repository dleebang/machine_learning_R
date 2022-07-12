# Quadratic discriminant analysis (QDA) is a version of Naive Bayes in which
# we assume that the distributions p_X|Y=1(x) and p_X|Y=0(x) are multivariate
# normal.
# 
# QDA can work well with a few predictors, but it becomes harder to use as the
# number of predictors increases. Once the number of parameters approaches the size
# of our data, the method becomes impractical due to overfitting.
# 
#
## Code
library(tidyverse)
library(caret)
library(dslabs)

# Load data
data("mnist_27")

# Estimate parameters from the data for each predictor assuming each one is bivariate normal
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

# Contour plots showing the estimated normal densities (contour plots representing
# 95% of the points)
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# We can use train function from caret package to fit the model and obtain predictors
train_qda <- train(y ~., method = "qda", data = mnist_27$train)

# Obtain predictors and accuracy
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# The estimated conditional probability looks relatively good, although it does
# not fit as well as the kernel smoothers (see graphics in https://rafalab.github.io/dsbook/examples-of-algorithms.html#quadratic-discriminant-analysis)
# On reason QDA does not work as well as the kernel methods is perhaps because the assumption
# of normality does not quite hold.

# Draw separate plots for 2s and 7s
# note that points for 7s have a somehow curved distribution
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

# QDA can work well here, but it becomes harder as the number of predictors increases.
# Once the number of parameters approaches the size of the data, the method becomes
# impractical due to overfitting.
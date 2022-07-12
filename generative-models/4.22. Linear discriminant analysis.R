# A relatively simple solution to the problem of having too many parameters is
# to assume that the correlation structure is the same for all classes, which
# reduces the number of parameters we need to estimate.
# 
# Forcing the assumption that all predictors share the same standard deviations
# and correlations, the boundary between predictors will be a line, just
# as with logistic regression. For this reason, we call the method as
# Linear discriminant analysis (LDA). 
# 
# In the case of LDA, the lack of flexibility does not permit us to capture
# the non-linearity of the predictors in the true conditional
# probability function.
# 
##Code

#get estimates = just one pair of standard deviations and one correlation
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))

#force sds and corr coefficients to be the same among all predictors
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))

#now the sizes of the ellipses as well as the angle are the same among 2s and 7s
#see in https://rafalab.github.io/dsbook/examples-of-algorithms.html#quadratic-discriminant-analysis
#This is because they have now the same sds and correlations

#Fit the Linear discriminant analysis:
train_lda <- train(y ~., method = "lda", data = mnist_27$train)

#get predictions
y_hat <- predict(train_lda, mnist_27$test)

#get accuracy
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

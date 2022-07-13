## Need to run code from previous scripts.

# The general idea behind regularization is to constrain the total variability
# of the effect sizes. So, instead of minimizing the least squares equation,
# we minimize an equation that adds a penalty term, with th first term as the
# sum of squares and the second a penalty (lambda) that gets larger when many
# b_i are large. 
# 
# This approach will have our desired effect: when our sample size n_i (number
# of ratings made for movie i) is very large, a case which will give us a stable
# estimate, then the penalty lambda is effectively ignored since
# n_i + lambda is approximately equals to n_i. However, when the n_i is small
# then the estimate b_hat_i(lambda) is shrunken towards 0. The larger lambda,
# the more we shrink. 
# 
# Lets compute the regularized estimates of b_i using lambda = 3. Later, we will
# see why the 3.
lambda <- 3
mu <- mean(train_set$rating)

movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

# To see how the estimates shrink, lets make a plot of regularized estimates
# vs. the least squares estimates. Note how estimates with small sample sizes
# shrink towards 0.
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

#Now, lets look at the top 10 movies based on the penalized estimates b_hat_i(lambda)
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  knitr::kable()

#These make much more sense, these movies are watched more and have more ratings.
#Now, the top 10 worst movies:
train_set %>%
  count(movieId) %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

#Did we improve our RMSE, now?
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

#The penalized estimates provide a large improvement over the least squares
#estimates
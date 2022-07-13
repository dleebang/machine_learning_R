# Note that the lambda penalty term included in the least square equations is a
# tuning parameter, and therefore we can use cross-validation to choose it.

lambdas <- seq(0, 10, 0.25)

mu <- mean(train_set$rating)

just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)] #This is the explanation of why we use lambda = 3, it minimizes the RMSE

#However, this is just an illustration, in practice we should be using full
#cross-validation just on the TRAIN set. The TEST set should never be
#used for tuning. 

#We can use regularization to estimate user effects as well.
#Besides including the b_u term in the least squares equation, we also
#include a penalty term for users.
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  #estimate movie effect with penalizations
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  #estimate user effect with penalizations
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #make predictions
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))

rmse_results %>% knitr::kable(digits = 3, "simple")

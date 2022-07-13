# To improve our results, we will use REGULARIZATION. Regularization constrains the
# total variability of the effect sizes by PENALIZING LARGE ESTIMATES THAT COME
# FROM SMALL SAMPLE SIZES.
# 
# To estimate the b`s, we will now minimize the least square estimate equation, 
# by including a penalty term:
# 
#     See formula in https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+2T2021/block-v1:HarvardX+PH125.8x+2T2021+type@sequential+block@a5bcc5177a5b440eb3e774335916e95d/block-v1:HarvardX+PH125.8x+2T2021+type@vertical+block@0f5cd79d0f374106a640b63f2c82d56a
#     
# The penalty term includes a lambda term. The larger the lambda is, the more
# we shrink the estimate to 0. The lambda is a tuning parameter, so we can use
# cross-validation to choose it. We should be using full cross-validation
# on just the TRAINING SET, without using the test set until the final assessment.
# 
# We can also use regularization to estimate the user effect by minimizing
# the equation that estimates least squares by adding the penalty term.
# 
#   See formula in https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+2T2021/block-v1:HarvardX+PH125.8x+2T2021+type@sequential+block@a5bcc5177a5b440eb3e774335916e95d/block-v1:HarvardX+PH125.8x+2T2021+type@vertical+block@0f5cd79d0f374106a640b63f2c82d56a
# 
# 
## Code
library(dslabs)
library(tidyverse)
library(caret)
data("movielens")
set.seed(755, sample.kind = "Rounding")


##### CODES FROM PREVIOUS SCRIPTS ##### 
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
test_set <- movielens[test_index,]
train_set <- movielens[-test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#define rmse calculation function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#just the average model to minimize LSE
mu_hat <- mean(train_set$rating)
naive_rmse <- RMSE(test_set$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(train_set$rating) 

#movie effect model
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))

#user effect model
user_avgs <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
       left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))
##### END OF CODES FROM PREVIOUS SCRIPTS ##### 

#explore the 10 largest mistakes (which movies have the largest residuals)
test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

#Lets look at the top 10 worst and 10 best movies based on b_hat_i
#First connect movieId to movie title
movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

#10 top movies according to our estimates:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

#10 worst movies:
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

#they all seem quite obscure. Lets look how often they are rated:
#top 10
train_set %>% count(movieId) %>% 
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10) %>% 
  knitr::kable()

#10 worst
train_set %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10) %>% 
  knitr::kable()

# the supposed 10 best and worst movies were rated by very few users, in most
# cases just 1. This is because with just a few users rating, we have more
# uncertainty. Therefore, larger estimates of b_i, negative or positive, are
# more likely. These are noisy estimates that we should not trust, especially
# when it comes to predicition. Large errors can increase our RMSE, so we would
# rather be conservative when unsure.
# 
# Regularization permits us to penalize large estimates coming from small
# sample sizes. It has commonalities with the Bayesien approach that shrunk
# predictions to 0. 
#The algorithms we saw up to now are examples of supervised machine learning. The
#name comes from the fact that we use the outcomes in a training set 
#to supervise the creation of our prediction algorithm.
#
#There is another subset of ML algorithms referred to as UNSUPERVISED. These are
#also called to as clustering algorithms since predictors are used to define
#clusters. 
#
#There are applications in which unsupervised learning can be a powerful technique,
#in particular as an exploratory tool. 
#
#A first step in any clustering algorithm is to DEFINE A DISTANCE BETWEEN OBSERVATIONS
#or GROUPS OF OBSERVATIONS. Here we introduce two examples: 
#  
#   hierarchical clustering
#   k-means
# 
#We will construct a simple example based on movie ratings. Here we construct
#a matrix x that has ratings for the 50 movies with the most ratings.

##Code
library(tidyverse)
library(dslabs)
data("movielens")

#get index of top 50 movies with most ratings
top <- movielens %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

#make a dataframe with each rating given by users for the movies
x <- movielens %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% 
  select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()

#remove col means and row means to center the observations
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))

rownames(x) <- row_names

#Use that data to find out if there are clusters of movies based on the ratings
#of 139 raters. A first step is to find the distance between each pair of movies
#using the dist function
d <- dist(x)


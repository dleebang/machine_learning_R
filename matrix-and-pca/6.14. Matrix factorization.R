# Our earlier models fail to account for an important source of variation
# related to the fact that groups of movies and groups of users have similar rating
# patterns. We can observe these patterns by studying the residuals and CONVERTING
# OUR DATA INTO A MATRIX WHERE EACH USERT GETS A ROW AND EACH MOVIE GETS A COLUMN:
# 
#   r_u,i = y_u,i - b_hat_i - b_hat_u
#   
#   where y_u,i is the entry in row u and column i
# 
# We can FACTORIZE THE MATRIX OF RESIDUALS r into a vector p and vector q,
# r_u,i ~~ p_u*q_i, allowing us to explain more of the variance using a model like:
# 
#   Y_u,i = mu + b_i + b_u + p_u*q_i + epslon_i,j
#   
# Because our example is more complicated (netflix dataset), we can use
# two factors to explain the structure and two sets of coefficients to describe
# users: 
# 
#   Y_u,i = mu + b_i + b_u + p_u1*q_i1 + p_u2*q_i2+ epslon_i,j
# 
# To estimate factors using our data instead of constructing them oursevels,
# we can use principal component analysis (PCA) or singular value decomposition (SVD).
# 
#
##Code
library(tidyverse)
library(dslabs)
data("movielens")
#For illustrative puporses, we will consider a small subset of movies with
#many ratings and users that have rated many movies. We keep movie 3252 (Scent
#of a Woman) to use it in a specific example.
train_small <- movielens %>% 
  group_by(movieId) %>%
  filter(n() >= 50 | movieId == 3252) %>% ungroup() %>% 
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

#construct a matrix in which each row is a user and each column is a movieId
y <- train_small %>% 
  select(userId, movieId, rating) %>%
  pivot_wider(names_from = "movieId", values_from = "rating") %>%
  as.matrix()

#add rownames and columnnames
rownames(y)<- y[,1] #userId as rownames
y <- y[,-1] #remove userId column

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)]) #add movie ids as columns

#convert the matrix to residuals by removing the column and row effects
y <- sweep(y, 2, colMeans(y, na.rm=TRUE))
y <- sweep(y, 1, rowMeans(y, na.rm=TRUE))

#if the model above explains all the signals, and the epslon are just noise,
#then the residuals for diff. movies should be independet from each other.
#But they are not. Here are some examples:
m_1 <- "Godfather, The"
m_2 <- "Godfather: Part II, The"
p1 <- qplot(y[ ,m_1], y[,m_2], xlab = m_1, ylab = m_2)

m_1 <- "Godfather, The"
m_3 <- "Goodfellas"
p2 <- qplot(y[ ,m_1], y[,m_3], xlab = m_1, ylab = m_3)

m_4 <- "You've Got Mail" 
m_5 <- "Sleepless in Seattle" 
p3 <- qplot(y[ ,m_4], y[,m_5], xlab = m_4, ylab = m_5)

gridExtra::grid.arrange(p1, p2 ,p3, ncol = 3)

#Note that the residulas are very correlated depending on the movie genre.
#The plots above say that users that like Godfather more than the expected by
#the model (based on movie and user effect), also like other gangster movies such
#as godfather 2 or goodfellas. There is some correlation.

#By looking at the correlation between movies, we can see a pattern (rename
#columns to save print space):
short_names <- c("Godfather", "Godfather2", "Goodfellas",
                 "You've Got", "Sleepless")
x <- y[, c(m_1, m_2, m_3, m_4, m_5)]
colnames(x) <- short_names

cor(x, use = "pairwise.complete")


#Note the positive and negative correlations between diff. movies genres
#There seems to be people that like romantic comedies more than expected,
#while others like gangster ones more than expected.

#create Q and P vectors that explain the correlation structure we see
set.seed(1)
options(digits = 2)
Q <- matrix(c(1 , 1, 1, -1, -1), ncol=1)
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5)
P <- matrix(rep(c(2,0,-2), c(3,5,4)), ncol=1)
rownames(P) <- 1:nrow(P)

#multiply Q by P vectors using matrix multiplication
#%*% = matrix multiplication
X <- jitter(P%*%t(Q))
X %>% knitr::kable(align = "c")

cor(X)


#Now lets add some complication by adding Scenf of a Woman movie and
#adding another factor (now we have 2 factors)
set.seed(1)
options(digits = 2)
m_6 <- "Scent of a Woman"
Q <- cbind(c(1 , 1, 1, -1, -1, -1), 
           c(1 , 1, -1, -1, -1, 1))
rownames(Q) <- c(m_1, m_2, m_3, m_4, m_5, m_6)
P <- cbind(rep(c(2,0,-2), c(3,5,4)), 
           c(-1,1,1,0,0,1,1,1,0,-1,-1,-1))/2
rownames(P) <- 1:nrow(X)

X <- jitter(P%*%t(Q), factor=1)

cor(X)

six_movies <- c(m_1, m_2, m_3, m_4, m_5, m_6)
tmp <- y[,six_movies]
cor(tmp, use="pairwise.complete")

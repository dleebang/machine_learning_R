# Most clustering and machine learning techniques rely on being able to
# define distance between observations, using features or predictors.
# 
# With high dimensional data, a quick way to compute all the distances at once
# is to use the function dist(), which computes the distance between each row
# and produces and object of class dist().
# 
# We can also compute distances between predictors. If N is the number of
# observations, the distance between two predictors, say 1 and 2, is:
# 
#   dist(1,2) = sqrt(sum(x_1 - x_2)^2)
#   
# To compute the distance between all pairs of the 784 predictors (columns), we can
# transpose the matrix first and then use dist()
# 

## Code
library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0, sample.kind = "Rounding")
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

#three first digits
y[1:3]

#access three first rows
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row using dist()
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

#look at the distance between each pixel and the pixel 492 (random)
d_492 <- as.matrix(d)[492,]
image(1:28, 1:28, matrix(d_492, 28, 28))

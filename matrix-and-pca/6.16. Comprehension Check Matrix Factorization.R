https://learning.edx.org/course/course-v1:HarvardX+PH125.8x+2T2021/block-v1:HarvardX+PH125.8x+2T2021+type@sequential+block@a5bcc5177a5b440eb3e774335916e95d/block-v1:HarvardX+PH125.8x+2T2021+type@vertical+block@96a2aeac752c48fd8abb6a3a8db5a339

#In this exercise we will see one of the ways singular value decomposition can
#be useful. To do this, we will construct a dataset that represents grade scores
#for 100 students in 24 diff. subjects. The overall average has been removed
#so this data represents the percentage point each student received above or
#below the average test score. So a 0 represents an average grade (C), a 25 is a
#high grade (A+), and a -25 represents a low grade (F). 
#

set.seed(1987, sample.kind = "Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 

m <- MASS::mvrnorm(n, rep(0, 3), Sigma)

m <- m[order(rowMeans(m), decreasing = TRUE),]

y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)

colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
head(y)

#Our goal will be to describe the student performances as succinctly as possible.
#For example, we want to know if these test results are all just a random 
#independent numbers. Are all students just about as good? Does being good in
#one subject imply to be good in another? How does the SVD help with all this?
#
#We will go step by step to show that with just three relatively small pairs
#of vectors we can explain much of the variability in this 100x24 dataset


#Q1)
# you can visualize the 24 test scores for the 100 students by plotting an image:
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)
#The students that test well are at the top of the image and there seem to be three groupings by subject.


#Q2)
#We can examine the correlation between the test scores directly like this
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.


#Q3)
#Remember that orthogonality means that t(U)*U and t(V)*V are equal to the
#identity matrix. This implies that we can also rewrite the decomposition as
#YV = U*D or t(U)*Y = D*t(V)
#
#We can think of YV and t(U)*Y as two transformations of Y that preserve
#the total variability of Y since U and V are orthogonal.
#
#compute the SVD of y using svd() function.
s <- svd(y)
names(s)

#you can check that the SVD works by typing:
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

#compute the sum of squares of columns of Y, and the sum of squares of columns of
#the transformed YV
ss_y <- apply(y^2, 2, sum)
sum(ss_y)

ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_yv)

#note that the total sum of squares is preserved. This is because V is orthogonal.


#Q4)
#Now to understand how YV is useful, plot ss_y agains the column number and 
#then do the same for ss_yv
library(tidyverse)
p1 <- qplot(1:length(ss_y), ss_y) 
p2 <- qplot(1:length(ss_yv), ss_yv) 
gridExtra::grid.arrange(p1, p2, ncol=2, top = "Y x YV")

#We see that the variability of columns of YV is decreasing


#Q5)
#Now notice we didn't have to compute ss_yv because we already have the answer.
#How? Because Y*V = U*D and U is orthogonal, we know that the sum of squares
#of the columns of U*D are the diagonal entries of D squared. Confirm this
#by plotting the square root of ss_yv vs. the diagonal entries of D.
plot(sqrt(ss_yv), s$d)


#Q6)
#what proportion of the total variability is explained by the first three
#columns of YV or UD (which is D squared)?
sum(ss_yv[1:3]) / sum(ss_yv)
#or
sum(s$d[1:3]^2) / sum(s$d^2)

#we can see that almos 99% of the variability is explained by the first three
#columns of Y*V = U*D. So we get the sense that we should be able to explain
#much of the variability and structure we found while exploring the data
#with a few columns.


#Q7)
#we can use sweep function to use matrix multiplication to compute
#U*D without having to construct diag(s$d) or using %*% (matrix multiplication)
identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))


#Q8)
#We now know that the fisrt column of the matrix U*D has the most variability
#of all the columns of U*D. Earlier we looked at an image of Y using my_image(y)
#in which we saw that the student to student variability is quite large and
#students that are good in one subject tend to be good in all. 
#This implies that the average across all subjects for each student should
#explain a lot of the variability.
#
#Compute the average score for each student, plot it against first column of
#the matrix U*D
avg_score <- rowMeans(y)
U_D_matrix <- sweep(s$u, 2, s$d, FUN = "*") # or U_D_matrix <- s$d^2
plot(avg_score, U_D_matrix[,1])

#There is a linear relationship between the average score for each student and the first column of the matrix U*D


#Q9)
#We note that the signs in SVD are arbitrary, and with this in mind we see that
#the first column of U*D is almost identical to the average score for each
#student except for the sign. This implies that multiplying Y by the first
#column of V must be performing a similar operation to taking the average.
#
#Make a plot of the matrix V (s$v)
my_image(s$v)

#The first column is very close to being a constant, which implies that the first column of YV is the sum of the rows of Y multiplied by some constant, and is thus proportional to an average. 


#Q10)
names(s)
#make an image of U1 then plot t(V1) using the same range for the y-axis limits,
#then make an image of U1*D1*t(V1) and compare it to the image of Y. 
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)



#Q11)
#In exercise 6, we saw how to calculate the percent of total variability explained.
#However, our approximation only explains the observation that good students tend to
#be good in all subjects. Another aspect of the original data that our approximation does not
#explain was the higher similarity we observed within subjects.
#We can compute the difference between our approximation and original data and then
#compute correlations.

#diff between approximation (y) and original data
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
#plot correlation between residuals 
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#we have removed the overall student effect, but the correlation plot reveals
#that we have not yet explained the within subject correlation nor the fact
#that math and science are closer to each other than to the arts.

#So lets explore the second column of the SVD.
#Plot the second column U2, then t(V2) using the same range for the y-axis limit
#Then make an image of U2*D2*t(V2) and compare it to the image of resid
plot(s$u[,2], ylim = c(-0.5, 0.5))
plot(s$v[,2], ylim = c(-0.5, 0.5))
with(s, my_image((u[,2, drop=F] * d[2]) %*% t(v[,2, drop=F])))



#Q12)
# The second column clearly relates to a student's difference in ability in
# math/science versus arts. We can see this most clearly from the plot of s$v[,2].
# Adding the matrix we obtain these two columns will help with our approximation:
#
#   Y = D1*U1*t(V1) + D2*U2*t(V2)
#   
# We know it will explain sum(s$d[1:2]^2)/sum(s$d^2) * 100 (89%) percent of the 
# total variability: 
sum(s$d[1:2]^2)/sum(s$d^2) * 100

# We can compute the new residuals an plot it:
resid <- y - with(s, sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# We note that the structure that is left is driven by the differences
# between math and science. We can confirm this by plotting U3, t(V3) using the
# same y axis limits, then make an image of U3*D3*t(V3) and compare it to the
# image of residual. 
plot(s$u[,3], ylim = c(-0.5, 0.5))
plot(s$v[,3], ylim = c(-0.5, 0.5))
with(s, my_image((u[,3, drop=F] * d[3]) %*% t(v[,3, drop=F])))



#Q13)
# The third column clearly relates to a student's difference in ability in math
# and science. We can see this most clearly from the plot of s$v[,3]. Adding
# the matrix with these two columns will help with our approximation:
#
#   Y = D1*U1*t(V1) + D2*U2*t(V2) + D3*U3*t(V3)   
#   
# We know it will explain sum(s$d[1:3]^2)/sum(s$d^2) * 100 (98%) percent of the
# total variability.  
sum(s$d[1:3]^2)/sum(s$d^2) * 100

# We can compute new residuals like this:
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# We no longer see structure in the residuals: they seem to be independent
# of each other. This implies that we can describe the data with the following
# model:
#
#   Y = D1*U1*t(V1) + D2*U2*t(V2) + D3*U3*t(V3) + epslon
#   
# with epslon a matrix of independent identically distributed errors. This model
# is useful because we summarize of 100x24 observations with 3*(100+24+1) = 375 numbers.
# 
# Furthermore, the three components of the model have useful interpretations:
# 
#   1 - the overall ability of a student (1st column)
#   2 - the difference in ability between math/sciences and arts (2nd column)
#   3 - the remaining differences between the three subjects (3rd column)
#   
# The sizes of D1, D2, D3 tell us the variability explained by each component.
# Finally note that the components Dj*Uj*t(Vj) are equivalent to the jth principal
# component. 
# 
# Finish the exercise by plotting an image of Y, an image of D1*U1*t(V1) + D2*U2*t(V2) + D3*U3*t(V3)
# and an image of the residuals, all with the same zlim
y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(y, zlim = range(y)) #plot y
my_image(y_hat, zlim = range(y)) #plot y_hat (D1*U1*t(V1) + D2*U2*t(V2) + D3*U3*t(V3))
my_image(y - y_hat, zlim = range(y)) #plot residuals (y - y_hat)

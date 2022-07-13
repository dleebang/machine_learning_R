#Need to run code from the previous scripts

#K-means clustering algorithms use a pre-defined "k", the number of clusters
#we want to define. The k-means algorithm is iterative. The first step is to
#define k centers. Then each obsrvation is assigned to the cluster with the closest 
#center to that observation. In a second step the centers are redefined using
#the observation in each cluster: the COLUMN MEANS ARE USED TO DEFINE A CENTROID. 
#We repeat these two steps until the centers converge.
#
#The kmeans function included in R-base does not handle NAs. For illustrative
#purposes we will fill NAs with 0s. The choise of how to fill in missing data
#should be made with care:


## Code
x_0 <- x
x_0[is.na(x_0)] <- 0
k <- kmeans(x_0, centers = 10)

#The cluster assignments are in the cluster component
groups <- k$cluster

#Note that because the first center is chosen at random, the final clusters
#are random. We impose some stability by repeating the entire function
#several times and averaging the results. The number of random starting values
#to use can be assigned through the nstart argument.
k <- kmeans(x_0, centers = 10, nstart = 25)

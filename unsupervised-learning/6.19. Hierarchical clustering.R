#Need to run code from the previous scripts

#with the distance between each pari of movies computed, now we need an algorithm
#to define groups from these. Hierarchical clustering starts by defining each
#observation as a separate grup, then the two closes groups are joined into
#a group iteratively until there is just one group including all the
#observations.

#The hclust() function implements this and takes a distance as input
h <- hclust(d)

#We can see the resulting groups using a dendogram:
plot(h, cex = 0.75, main = "", xlab = "")

#This dendogram gives un an approximation btw the distance btw any two movies.
#To find this distance, we find the first location, from top to bottom, where
#these movies split into two diff groups. The height of this location is the distance
#btw these two groups. So, for example the distance btw the three star wars movies
#is 8 or less, while the distance btw Raiders of the Lost Ark and Silence of the Lambs
#is about 17.

#To generate actual groups we can do one of two things:
#
# 1) decide on a min. distance needed for observations to be in the same group
# 2) decide on the number of groups you want and then find the min. distance
# that achieves this. 
# 
#The function cutree() can be applied to the output of hclust to perform either
#of these two operations and generate groups.
groups <- cutree(h, k = 10)

#Note that the clustering provides some insights into types of movies.
#Group 4 appears to be blockbusters:
names(groups)[groups==4]

#And group 9 appers to be nerd movies:
names(groups)[groups==9]

#We can change the size of the group by either making "k" larger or "h" smaller.
#We can also explore the data to see if there are clusters of movie raters 
#by transposing the matrix (making users as rows of obs.)
h_2 <- dist(t(x)) %>% hclust()

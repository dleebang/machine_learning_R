# You can think of singular value decomposition (SVD) as an algorithm that
# find the vectors p and q described in the previous script that permit us 
# to write the matrix of residuals r with m rows and n columns in the following way:
# 
#   r_u,i (residuals) = p_u1*q_i1 + p_u2*q_i2 +...+ p_u_m*q_i_m
#   
# with the variability of these terms descreasing and the p's UNCORRELATED
# TO EACH OTHER
# 
# SVD also computes the variabilities so that we can know how much of the
# matrix's total variability is explained as we add new terms.
# 
# The VECTORS q ARE CALLED THE PRINCIPAL COMPONENTS and the VECTORS p ARE THE
# USER EFFECTS. By using principal component analysis (PCA), MATRIX FACTORIZATION
# CAN CAPTURE STRUCTURE IN THE DATA, in this particular case
# determined by user opinions about movies.
#
#
## Code
#substitute NA values with 0
y[is.na(y)] <- 0

#remove user effects
y <- sweep(y, 1, rowMeans(y))

#compute principal components
pca <- prcomp(y)

#the q vectors are called the principal components and they are stored in 
#this matrix:
dim(pca$rotation)

#while the p vectors, or the user effects, are here:
dim(pca$x)

#we can see the variability of each of the vectors:
qplot(1:nrow(x), pca$sdev, xlab = "PC")

#we can also notice how the first two PCs relate to the structure in opinions 
#about the movies:
library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

#we can note that by looking at the top 10 movies in each direction, we see a
#meaningful pattern. 
#The PC1 axis show movies critically acclaimed on the left side
pcs %>% select(PC1) %>% arrange(PC1) %>% slice(1:10)

#while on the right side there are hollywood blockbusters. 
pcs %>% select(PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

#The PC2 seems to go from artsy, independent films (below) 
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

#to nerd favorits (above)
pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)


#You can also plot how much cumulative principal components explained variability in the data
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

#note that about 50 PCs explain ca. 50% of variability in the data
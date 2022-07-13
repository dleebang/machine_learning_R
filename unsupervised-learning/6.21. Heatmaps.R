#A powerful visualization tool for discovergin clusters or patters in your data
#is the heatmap. The idea is simple: plot an image of your data matrix with colors
#used as the visual cue and both the columns and row ordered according to the results
#of a clustering algorithm. We will demonstrate this with the tissue_gene_expression dataset

#We will scale the rows (remove col means) of the gene expression matrix 
#if one wanted to scale columns, remove the row means
data("tissue_gene_expression")

x <- sweep(tissue_gene_expression$x, 2, colMeans(tissue_gene_expression$x))
h_1 <- hclust(dist(x))
h_2 <- hclust(dist(t(x)))

#Now we can use the results of this clustering to order the rows and columns
image(x[h_1$order, h_2$order])

#But there is a heatmap function that does it for us:
heatmap(x, col = RColorBrewer::brewer.pal(11, "Spectral"))

#There are too many features for the plot to be useful, so we will filter
#some columns and remake the plots. One simple approach is to remove features
#with no information and only include those with high variance. 
#(In the movie example, a user with low variance in their ratings is not really
#informative: all the movies seem about the same to them)

#Include only features with high variance:
library(matrixStats)
#get col standar deviations
sds <- colSds(x, na.rm = TRUE)
#get index of the 25 columns with highest variance
o <- order(sds, decreasing = TRUE)[1:25]

#build heatmap of these 25 columns
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))

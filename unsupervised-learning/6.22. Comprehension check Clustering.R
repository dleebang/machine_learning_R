library(tidyverse)
library(dslabs)
data("tissue_gene_expression")

#Q1)
#Load the tissue dataset and remove the row means, then compute the distance
#between each observation. Store in d.
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))



#Q2)
#Make a hierarchical clustering plot and add the tissue types as labels
#use hclust() to define clusters from distances between observations
h <- hclust(d)
plot(h, cex = 0.65, main = "", xlab = "")



#Q3)
#Select 50 most variable genes. Make sure observations show up in the columns,
#predictors are centered, and add color bar to show different tissue types.
library(RColorBrewer)
#compute columns standard deviations
sds <- matrixStats::colSds(tissue_gene_expression$x)
#take indexes of 50 columns with highest sds
ind <- order(sds, decreasing = TRUE)[1:50]
#assign colors
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
#build a heatmap
heatmap(t(tissue_gene_expression$x[,ind]), 
        col = brewer.pal(11, "RdBu"), 
        scale = "row", 
        ColSideColors = colors)

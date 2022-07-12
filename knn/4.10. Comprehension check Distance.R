library(dslabs)
data(tissue_gene_expression)
#dimension of matrix x: levels of gene expression (cols) vs. biological samples (rows)
dim(tissue_gene_expression$x)
#tissue types
table(tissue_gene_expression$y)

#Q1)
d <- dist(tissue_gene_expression$x)

#Q2)
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

#Q3)
image(as.matrix(d))

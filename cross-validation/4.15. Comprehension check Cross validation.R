library(tidyverse)
library(caret)

#Q1)
set.seed(1996, sample.kind="Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)

colnames(x) <- paste("x", 1:ncol(x), sep = "_")

y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

#Q2)
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

#Q3)
ind <- which(pvals <= 0.01)
length(ind)

#Q4)
x_subset <- x[,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

#Q5)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

#Q6)
#We used the entire dataset to select the columns used in the model, so the
#accuracy is too high. The selection step needs to be included as part of the
#cross-validation algorithm, and then the cross validation itself is performed
#after the column selection step.

#Q7)
library(dslabs)
data("tissue_gene_expression")
tissue_gene_expression

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y

fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
ggplot(fit)

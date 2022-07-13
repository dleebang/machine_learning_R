#We will now explore tissue_gene_expression predictors
library(tidyverse)
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)


#Q1)
#We wanto to get an idea of which observations are close to each other, but,
#as you can see from dimensions, the predictors are 500-dimensional, making plot
#difficult. We can transform the data to principal components and plot
#with tissues with diff colors.
pc <- prcomp(tissue_gene_expression$x)

#Which tissue is in a cluster by itself?
pcs <- data.frame(pc$x, tissue = tissue_gene_expression$y)
pcs %>% ggplot(aes(PC1, PC2, color = tissue)) +
  geom_point(cex = 3)
  

#Q2)
#The predictors for each observation are measured using the same device and
#experimental procedure. This introduces biases that can affect all predictors
#from one observation. For each observation in x, compute the average across all
#predictors, and then plot this agains the first PC with color representing tissue.
cor <- bind_cols(pred_avg = rowMeans(tissue_gene_expression$x), 
                 PC1 = pc$x[,1], 
                 tissue = tissue_gene_expression$y)

#Compute the correlation btw average across all predictors for each tissue and
#PC1
cor(cor$pred_avg, cor$PC1)

#plot
cor %>% ggplot(aes(PC1, pred_avg, col = tissue)) +
  geom_point(cex = 3)


#Q3)
#We see an association with the first PC and the observation averages.
#Redo the PCA but only after removing the center (mean)
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#For the 7th PC, which two tissues have the greatest median difference?
data.frame(pc$x[,1:10], tissue = tissue_gene_expression$y) %>% 
  ggplot(aes(tissue, PC7)) +
  geom_boxplot()
  
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}


#Q5)
plot(cumsum(pc$sdev^2/sum(pc$sdev^2)))
# or
plot(summary(pc)$importance[3,])


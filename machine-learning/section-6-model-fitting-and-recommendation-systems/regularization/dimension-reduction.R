library(dslabs)
library(tidyverse)
library(caret)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#Plot the first two principal components with color representing tissue type.
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#For each observation, compute the average across all predictors, and then plot 
#this against the first PC with color representing tissue. Report the correlation.
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


#For the first 10 PCs, make a boxplot showing the values for each tissue.
#For the 7th PC, which two tissues have the greatest median difference?
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#Plot the percent variance explained by PC number
plot(summary(pc)$importance[3,])
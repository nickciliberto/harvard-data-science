library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- as.matrix(dist(tissue_gene_expression$x))
slice <- c(1, 2, 39, 40, 73, 74)
d[slice, slice]
image(d)
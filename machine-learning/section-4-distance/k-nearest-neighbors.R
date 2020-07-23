library(dslabs)
library(caret)
library(tidyverse)
data(heights)
set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     
k <- seq(1, 101, 3)
scores <- sapply(k, function(k) {
  knn_fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class") %>%
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat_knn, reference = test_set$sex)
})

plot(k, scores)
max(scores)
k[which.max(scores)]

set.seed(1)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})
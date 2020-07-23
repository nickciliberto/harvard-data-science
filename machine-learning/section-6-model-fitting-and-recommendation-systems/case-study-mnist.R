models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", 
            "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(dplyr)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))

acc <- colMeans(pred == mnist_27$test$y)
mean(acc)

ens <- rowMeans(pred == "7")
ens_pred <- ifelse(ens > 0.5, "7", "2")
mean(ens_pred == mnist_27$test$y)

acc

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

data("tissue_gene_expression")
dim(tissue_gene_expression$x)
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

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

for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

plot(summary(pc)$importance[3,])
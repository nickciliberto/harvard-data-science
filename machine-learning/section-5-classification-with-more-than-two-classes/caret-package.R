library(rpart)
library(caret)
library(dslabs)
library(dplyr)

set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))

ggplot(fit)

set.seed(1991)
data("tissue_gene_expression")

fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), 
                  control = rpart.control(minsplit = 0)))

ggplot(fit)
confusionMatrix(fit)

plot(fit$finalModel)
text(fit$finalModel)

set.seed(1991)
fit_rf <- with(tissue_gene_expression, 
               train(x, y, method = "rf",
                     nodesize = 1,
                     tuneGrid = data.frame(mtry = seq(50, 200, 25))))
fit_rf$bestTune
ggplot(fit_rf)

imp <- varImp(fit_rf)
imp

data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)
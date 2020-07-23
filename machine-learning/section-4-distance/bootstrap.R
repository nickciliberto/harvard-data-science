library(dslabs)
library(caret)
data(mnist_27)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)
sum(sapply(1:10, function(x) {
  sum(indexes[[x]] == 3)
}))

set.seed(1)
values <- replicate(10000, {y <- rnorm(100, 0, 1)
quantile(y, 0.75)
})
mean(values)
sd(values)

set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)
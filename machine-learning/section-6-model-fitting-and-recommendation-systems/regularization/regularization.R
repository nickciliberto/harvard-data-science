library(dslabs)
library(tidyverse)
library(caret)

options(digits=7)
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1)
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#What are the top schools based on the average score? Show just the ID, size, 
#and the average score.
schools %>% top_n(10, score) %>% select(id, size, score) %>% arrange(desc(score))

#Compare the median school size to the median school size of the top 10 schools 
#based on the score.
median(schools$size)
schools %>% top_n(10, score) %>% .$size %>% median()

#What is the median school size of the bottom 10 schools based on the score?
schools %>% top_n(-10, score) %>% .$size %>% median()

#Plot the average score versus school size to see what's going on. 
#Highlight the top 10 schools based on the true quality.
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2)

overall <- mean(sapply(scores, mean))

#regularization with alpha = 25
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))

#What is the ID of the top school with regularization?
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#What alpha gives minimum RMSE?
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]

#Rank the schools based on the average obtained with the best alpha
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Re-run the code from the exercise in Q6 but without removing the overall mean.
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]
set.seed(16)
act_scores <- rnorm(10000, 20.9, 5.7)

#Question 1
mean(act_scores)
sd(act_scores)
sum(act_scores >= 36)
mean(act_scores >= 30)
mean(act_scores <= 10)

#Question 2
x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

#Question 3
act_mean <- mean(act_scores)
act_sd <- sd(act_scores)
1 - pnorm(act_mean + 2 * act_sd, act_mean, act_sd)

z_scores <- (act_scores - mean(act_scores))/sd(act_scores)

20.9 + 2 * 5.7

qnorm(.975, act_mean, act_sd)

#Question 4
qnorm(.95, act_mean, act_sd)
qnorm(.95, 20.9, 5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, p)
names(sample_quantiles[max(which(sample_quantiles < 26))])

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
p <- seq(0.01, 0.99, 0.01)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
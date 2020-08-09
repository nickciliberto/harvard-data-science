#Question 1

#What is the expected value of points for guessing on one question?
correct <- 1
wrong <- -.25
options <- 5
questions <- 44
p_correct <- 1/options
p_wrong <- 1 - p_correct
mu <- (correct * 1/options) + (wrong * (options - 1)/options)
mu

#What is the standard error of guessing on all 44 questions?
sigma <- (abs(correct - wrong) * sqrt((1/options) * (options - 1)/options)) * 
  sqrt(questions)
sigma

#Use the Central Limit Theorem to determine the probability that a guessing 
#student scores 8 points or higher on the test.
1 - pnorm(8, mu, sigma)

#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students 
#guessing on the test.
#What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
guess_scores <- replicate(10000, {student <- sample(c(1, -.25), questions, replace = TRUE, 
                                    prob = c(p_correct, p_wrong))
  sum(student)
})
mean(guess_scores >= 8)

#Question 2

#What is the expected value of the score when guessing on this new test?
correct <- 1
wrong <- 0
options <- 4
questions <- 44
p_correct <- 1/options
p_wrong <- 1 - p_correct
mu <- (correct * 1/options) + (wrong * (options - 1)/options)
mu * questions

#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
#representing a range of student skills.
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- questions * correct*x + wrong*(1-x)
  sigma <- sqrt(questions) * abs(correct - wrong) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

#Question 3
#What is the expected value of the payout for one bet?
winnings <- 6
wager <- 1
p_win <- 5/38
x_val <- p_win * winnings - (1-p_win) * wager

#What is the standard error of the payout for one bet?
s_e <- abs(-1 - 6) * sqrt(p_win*(1-p_win))
s_e

#What is the expected value of the average payout over 500 bets?
#Same as 1st part, trick question
winnings <- 6
wager <- 1
p_win <- 5/38
x_val <- p_win * winnings - (1-p_win) * wager

#What is the standard error of the average payout over 500 bets?
s_e / sqrt(500)

#What is the expected value of the sum of 500 bets?
x_val * 500

#What is the standard error of the sum of 500 bets?
s_e * sqrt(500)

#Use pnorm() with the expected value of the sum and standard error of the sum
#to calculate the probability of losing money over 500 bets,
#\( \mbox{Pr}(X \leq 0) \).
mu <- x_val * 500
sigma <- s_e * sqrt(500)
pnorm(0, mu, sigma)
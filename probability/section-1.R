library(gtools)
library(tidyverse)
library(dplyr)

#How many different ways can the 3 medals be distributed across 8 runners?
medals <- permutations(8,3)
nrow(medals)

#How many different ways can the three medals be distributed among the 3 runners from Jamaica?

medals_jamaica <- permutations(3,3)
nrow(medals_jamaica)

#What is the probability that all 3 medals are won by Jamaica?
nrow(medals_jamaica)/nrow(medals)

#Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B <- 10000

all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)

#How many meal combinations are possible with the current menu?
entrees <- 6
sides <- length(combinations(6, 2))/2
drinks <- 2
entrees * sides* drinks

#How many combinations are possible if he expands his original special to 3 drink options?
entrees <- 6
sides <- length(combinations(6, 2))/2
drinks <- 3
entrees * sides* drinks

#How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
entrees <- 6
sides <- length(combinations(6, 3))/3
drinks <- 3
entrees * sides* drinks

#- Write a function that takes a number of entree choices and returns the 
#number of meal combinations possible given that number of entree options,
#3 drink choices, and a selection of 2 sides from 6 options.

#What is the minimum number of entree options required in order to generate
#more than 365 combinations?
lunch_options <- function(entrees) {
  sides <- length(combinations(6, 2))/2
  drinks <- 3
  entrees * sides* drinks
}
sapply(1:12, lunch_options)

#- Write a function that takes a number of side choices and returns the number 
#of meal combinations possible given 6 entree choices, 3 drink choices, and a 
#selection of 2 sides from the specified number of side choices.

#What is the minimum number of side options required in order to generate more 
#than 365 combinations?
lunch_options <- function(num_sides) {
  entrees <- 6
  drinks <- 3
  sides <- length(combinations(num_sides, 2))/2
  entrees * sides* drinks
}
sapply(2:12, lunch_options)

#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use, part 1

#How many groups are in the study?
nrow(esoph)

#How many cases are there?
all_cases <- sum(esoph$ncases)

#How many controls are there?
all_controls <- sum(esoph$ncontrols)

#What is the probability that a subject in the highest alcohol consumption 
#group is a cancer case?
esoph %>%
  filter(alcgp == "120+") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)

#What is the probability that a subject in the lowest alcohol consumption 
#group is a cancer case?
esoph %>%
  filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_case = ncases / (ncases + ncontrols)) %>%
  pull(p_case)
#ALTERNATIVE SOLUTION
sum(esoph$ncases[esoph$alcgp == "0-39g/day"]) / (sum(esoph$ncases[esoph$alcgp == "0-39g/day"]) + sum(esoph$ncontrols[esoph$alcgp == "0-39g/day"]))

#Given that a person is a case, what is the probability that they smoke 10g 
#or more a day?
tob_cases <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncases) %>%
  sum()

tob_cases/all_cases

#Given that a person is a control, what is the probability that they smoke 10g 
#or more a day?
tob_controls <- esoph %>% filter(tobgp != "0-9g/day") %>% pull(ncontrols) %>% sum()
tob_controls/all_controls

#Questions 5 and 6: Esophageal cancer and alcohol/tobacco use, part 2

#For cases, what is the probability of being in the highest alcohol group?
alc_cases <- esoph %>% filter(alcgp == "120+") %>% pull(ncases) %>% sum()
alc_cases/all_cases

#For cases, what is the probability of being in the highest tobacco group?
highest_tob_cases <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

highest_tob_cases/all_cases

#For cases, what is the probability of being in the highest alcohol group and 
#the highest tobacco group?
high_alc_tob_cases <- esoph %>%
  filter(alcgp == "120+" & tobgp == "30+") %>%
  pull(ncases) %>%
  sum()

p_case_high_alc_tob <- high_alc_tob_cases/all_cases
p_case_high_alc_tob

#For cases, what is the probability of being in the highest alcohol group or 
#the highest tobacco group?
((highest_tob_cases + alc_cases)/all_cases) - p_case_high_alc_tob

#For controls, what is the probability of being in the highest alcohol group?
alc_controls <- esoph %>% filter(alcgp == "120+") %>% pull(ncontrols) %>% sum()
alc_controls/all_controls

#How many times more likely are cases than controls to be in the highest alcohol
#group?
(alc_cases/all_cases)/(alc_controls/all_controls)

#For controls, what is the probability of being in the highest tobacco group?
highest_tob_controls <- esoph %>%
  filter(tobgp == "30+") %>%
  pull(ncontrols) %>%
  sum()

highest_tob_controls/all_controls

#For controls, what is the probability of being in the highest alcohol group 
#and the highest tobacco group?
high_alc_tob_controls <- esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  pull(ncontrols) %>% sum()
high_alc_tob_controls/all_controls

#For controls, what is the probability of being in the highest alcohol group or 
#the highest tobacco group?
(alc_controls/all_controls) + (highest_tob_controls/all_controls) - high_alc_tob_controls/all_controls

#How many times more likely are cases than controls to be in the highest 
#alcohol group or the highest tobacco group?
controls <- (alc_controls/all_controls) + (highest_tob_controls/all_controls) - 
  high_alc_tob_controls/all_controls
cases <- ((highest_tob_cases + alc_cases)/all_cases) - p_case_high_alc_tob
cases / controls
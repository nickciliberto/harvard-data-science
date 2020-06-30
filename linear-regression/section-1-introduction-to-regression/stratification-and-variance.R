set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#Calculate the mean and standard deviation of mothers' heights, the mean and 
#standard deviation of daughters' heights, and the correlaton coefficient 
#between mother and daughter heights.
mother_avg <- mean(female_heights$mother)
mother_sd <- sd(female_heights$mother)
daughter_avg <- mean(female_heights$daughter)
daughter_sd <- sd(female_heights$daughter)
mother_to_daughter_corr <- cor(female_heights$mother, female_heights$daughter)

#Calculate the slope and intercept of the regression line predicting daughters' 
#heights given mothers' heights. Given an increase in mother's height by 1 
#inch, how many inches is the daughter's height expected to change?
slope <- mother_to_daughter_corr * daughter_sd / mother_sd
intercept <- daughter_avg - slope * mother_avg

#What percent of the variability in daughter heights is explained by the 
#mother's height?
mother_to_daughter_corr^2 * 100

#A mother has a height of 60 inches.
#What is the conditional expected value of her daughter's height given the 
#mother's height?
slope * 60 + intercept
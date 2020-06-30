library(Lahman)
library(tidyverse)
library(broom)
data("Teams")

Teams %>% filter(yearID == 1971) %>% lm(R~BB + HR, data = .) %>% tidy()

Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(tidy(lm(R~BB + HR, data = .))) %>%
  filter(term == "BB") %>%
  ggplot(aes(x = yearID, y = estimate)) +
  geom_point() + 
  geom_smooth(method = lm, se = TRUE)

Teams %>% filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  do(tidy(lm(R~BB + HR, data = .))) %>%
  filter(term == "BB") %>%
  lm(estimate~yearID, data = .) %>% 
  tidy()
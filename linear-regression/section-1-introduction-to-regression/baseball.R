library(Lahman)
library(tidyverse)

Teams <- Teams %>% filter(yearID %in% 1961:2001)

Teams <- Teams %>% mutate(AB_per_game = AB/G, R_per_game = R/G)
Teams %>% ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams <- Teams %>% mutate(win_rate = W/G, E_per_game = E/G)
Teams %>% ggplot(aes(win_rate, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams <- Teams %>% mutate(triples_per_game = X3B/G, doubles_per_game = X2B/G)
Teams %>% ggplot(aes(triples_per_game, doubles_per_game)) + 
  geom_point(alpha = 0.5)

#Part 2 Questions 7-9
cor(Teams$R/Teams$G, Teams$AB/Teams$G)
cor(Teams$W/Teams$G, Teams$E/Teams$G)
cor(Teams$X3B/Teams$G, Teams$X2B/Teams$G)
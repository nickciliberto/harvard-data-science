library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master, playerID = playerID) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

Salaries

top_salary <- Salaries %>% filter(yearID == 2016) %>% 
  right_join(top_names, playerID = playerID) %>%
  select(nameFirst, nameLast, teamID, HR, salary)

awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)
length(intersect(awards_2016$playerID, top_names$playerID))
length(setdiff(awards_2016$playerID, top_names$playerID))

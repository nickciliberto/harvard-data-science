library(Lahman)
Teams <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(R_per_G=R/G, BB_per_G=BB/G, HR_per_G=HR/G)

lm(R_per_G ~ BB_per_G + HR_per_G, data=Teams)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = female_heights)
female_heights$daughter[1] * fit$coef[2] + fit$coef[1]
female_heights$mother[1]

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
sum(bat_99_01$mean_singles > 0.2)

bat_99_02 <- inner_join(bat_99_01, bat_02)
cor(bat_99_02$singles, bat_99_02$mean_singles)
cor(bat_99_02$bb, bat_99_02$mean_bb)

bat_99_02 %>% ggplot(aes(mean_singles, singles)) + geom_point(alpha=0.5)
bat_99_02 %>% ggplot(aes(mean_bb, bb)) + geom_point(alpha=0.5)

singles_fit <- lm(singles ~ mean_singles, data = bat_99_02)
bb_fit <- lm(bb ~ mean_bb, data = bat_99_02)

singles_fit
bb_fit
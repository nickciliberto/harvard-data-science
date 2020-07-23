library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

#Compute the number of ratings for each movie and then plot it against the 
#year the movie came out. Use the square root transformation on the y-axis when 
#plotting.
#What year has the highest median number of ratings?
movielens %>% group_by(movieId) %>% 
  summarise(num_ratings = n(), year = as.character(first(year))) %>% 
  qplot(year, num_ratings, data = ., geom = "boxplot") + 
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Among movies that came out in 1993 or later, select the top 25 movies with 
#the highest average number of ratings per year (n/year), and calculate the 
#average rating of each of them. To calculate number of ratings per year, 
#use 2018 as the end year.
ratings_per_movie <- movielens %>% filter(year >= 1993 & year <= 2018) %>%
  group_by(movieId) %>%
  summarise(num_ratings = n())

inner_join(movielens, ratings_per_movie, movieId=movieId) %>%
  mutate(years = 2018-year, ratings_per_year = num_ratings/years) %>%
  group_by(movieId, title = title, ratings_per_year) %>%
  summarise(avg_rating = mean(rating)) %>%
  arrange(desc(ratings_per_year)) %>%
  top_n(25, ratings_per_year)

###CORRECT ANSWER
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))
###

#Stratify the post-1993 movies by ratings per year and compute their average 
#ratings. To calculate number of ratings per year, use 2018 as the end year. 
#Make a plot of average rating versus ratings per year and show an estimate of 
#the trend.
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

movielens <- mutate(movielens, date = as_datetime(timestamp))

#Compute the average rating for each week and plot this average against date.
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(date, avg_rating)) +
  geom_point() +
  geom_smooth()

#Define a category as whatever combination appears in this column. Keep only 
#categories with more than 1,000 ratings. Then compute the average and standard
#error for each category. Plot these as error bar plots.
movielens %>% group_by(genres) %>%
  filter(n() > 1000) %>%
  summarise(avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
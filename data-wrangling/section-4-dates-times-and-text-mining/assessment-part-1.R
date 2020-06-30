library(dslabs)
library(lubridate)
library(dplyr)
options(digits = 3)    # 3 significant digits

data(brexit_polls)

brexit_polls %>% filter(month(startdate) == 4) %>% nrow()
brexit_polls %>% filter(round_date(enddate, unit="week") == "2016-06-12") %>%
  nrow()

brexit_polls %>% mutate(ending_weekday = weekdays(enddate)) %>% 
  group_by(ending_weekday) %>%
  summarize(count = n())

data(movielens)
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))
names(which.max(reviews_by_year))

reviews_by_hour <- table(hour(dates))
names(which.max(reviews_by_hour))
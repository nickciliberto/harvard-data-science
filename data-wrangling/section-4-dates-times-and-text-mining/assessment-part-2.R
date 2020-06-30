library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

table(str_detect(gutenberg_metadata$title, "Pride and Prejudice"))

id <- gutenberg_works(title == "Pride and Prejudice", languages = c("en"))$gutenberg_id
book <- gutenberg_download(id)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)

words <- anti_join(words, stop_words)
nrow(words_non_stop)

words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

top_words <- words %>% group_by(word) %>% 
  summarize(count = n()) %>% 
  filter(count >= 100) %>% 
  arrange(desc(count))
top_words
count(top_words )

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words, afinn)
nrow(afinn_sentiments)
mean(afinn_sentiments$value > 0)
sum(afinn_sentiments$value == 4)
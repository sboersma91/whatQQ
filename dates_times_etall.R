library(tidyverse)
library(dslabs)
library(ggplot2)
library(stringr)
library(lubridate)
library(dslabs)


data("brexit_polls")
brexit_polls %>% filter(month(startdate) == "4")
head(brexit_polls)
brexit_polls %>% filter(round_date(enddate, unit = "week") == "2016-06-12")
#use sum function to get a total for the coding ggwp
days_all <- weekdays(brexit_polls$enddate)
n <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
brexit_polls %>% filter(weekdays(enddate) == "Monday")
table(weekdays(brexit_polls$enddate))
sum(days_all == "Sunday")

data("movielens")
which.max(table(as_datetime(movielens$timestamp) %>% hour()))


library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata                 

filter(gutenberg_metadata, str_detect(gutenberg_metadata$title, "Pride and Prejudice"))
gutenberg_works(title == "Pride and Prejudice")
book <- gutenberg_download(gutenberg_id = 1342)
words2 <- book %>% unnest_tokens(words, text)
nrow(words)

book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)


words <- words %>% anti_join(stop_words)
z <- words %>% filter(!str_detect(words$word, "\\d+"))
nrow(z)
x <- z %>% count(word, sort = TRUE)
t <- x %>% filter(n > 100)
nrow(t)
which.max(t$n)$t

afinn <- get_sentiments("afinn")
words6 <- words %>% inner_join(afinn)
pos <- words6 %>% filter(value > 0)
neg <- words6 %>% filter(value < 0)
nrow(pos)/nrow(words6)




















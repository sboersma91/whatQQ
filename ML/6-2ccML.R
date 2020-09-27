library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

head(movielens)
?movielens
sum(movielens$movieId == 31)

# attempts at question 1 ~ 1994

q1 <- movielens %>% 
        group_by(movieId, year) %>% 
        summarize(rating = n())
q1 %>% 
  ggplot(aes(year, rating)) +
  geom_boxplot()

w <- movielens %>% group_by(movieId) %>% 
  summarize(n_ratings = sqrt(n()), year = year) %>% 
  ungroup(movieId) %>% 
  group_by(year)
nrow(w)
sum(w$year == 1995)


w <- w %>% filter(year %in% 1995)
w
w$year <- as.numeric(w$year)
class(w$year)

median(w$n_ratings)

# years = median \\ 1995 = 8.94 below is the "correct" code sigh. larry 
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

library(dslabs)
library(tidyverse)
data("movielens")

head(movielens)

movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

keep <- movielens %>%
  dplyr::count(movieId) %>%
  top_n(5) %>%
  pull(movieId)
tab <- movielens %>%
  filter(userId %in% c(13:20)) %>% 
  filter(movieId %in% keep) %>% 
  select(userId, title, rating) %>% 
  spread(title, rating)
tab %>% knitr::kable()


na.rm(tab$`Shawshank Redemption, The`)

movielens %>% 
  filter(title == "Shawshank Redemption, The" & year < "2019") %>% 
  summarize(avg = mean(rating))


movielens %>% 
  filter(year >= 1993) %>% 
  group_by(title) %>% 
  summarize(n_of_ratings = n(), year = year, yeardiff = 2018 - year) %>% 
  group_by(title, year) %>% 
  summarize(medians = median(n_of_ratings/yeardiff)) %>% 
  group_by(title) %>% 
  arrange(desc(medians)) %>%
  top_n(25)


head(movielens)

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))


movielens %>% 
  group_by(genres) %>% 
  summarize(n = n(), av = mean(rating), se = sd(rating)/sqrt(n())) %>% 
  filter(n >= 1000) %>% 
  ggplot(aes(x = genres, y = av, ymin = av - 2*se, ymax = av + 2*se)) + 
  geom_point() + 
  geom_errorbar()

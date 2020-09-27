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




p <- w %>% group_by(year)
p
w
identical(w,p)

# %>% 
#   group_by(year) %>% 
#   summarize(med = median(n_ratings)) %>% 
#   arrange(desc(med))



z<-movielens %>%
  group_by(movieId) %>%
  summarize(n_ratings = sqrt(n()), year = year) %>% 
  group_by(year) %>% 
  summarize(quest = median(n_ratings), year = year)
z[which.max(z$quest),]

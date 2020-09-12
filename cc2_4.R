library(Lahman)
library(tidyverse)
library(dslabs)
library(broom)

pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean

pa_per_game
head(Batting$G)
head(Batting)
?Batting
Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G))

fit <- Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

Teams %>% 
  filter(yearID %in% 1971) %>% 
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G) %>% 
  do(tidy(lm(R ~ BB + HR, data = .))) %>% 
  filter(term == "HR" & p.value<.05)

playing<- Teams %>% 
    filter(yearID %in% 1961:2018) %>% 
    group_by(yearID) %>%
    mutate(BB = BB / G, 
           HR = HR / G,
           R = R / G) %>%
    select(yearID, BB, HR, R) %>% 
    do(tidy(lm(R ~ BB + HR, data = .))) #%>% 
    # filter(term == "BB")

head(playing)


playing %>% 
  ggplot(aes(yearID, estimate, col = term)) +
  geom_point() +
  geom_smooth(method = "lm")
  
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

head(res)

res %>% 
  filter(term == "BB") %>% 
  do(tidy(lm(estimate ~ yearID, data = .)))

























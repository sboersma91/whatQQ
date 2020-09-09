library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


# Teams %>% filter(yearID %in% 1961:2001) %>%
#   mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
#   ggplot(aes(HR_per_game, R_per_game)) + 
#   geom_point(alpha = 0.5)
# Teams %>% filter(yearID %in% 1961:2001) %>%
#   mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
#   ggplot(aes(SB_per_game, R_per_game)) + 
#   geom_point(alpha = 0.5)
# Teams %>% filter(yearID %in% 1961:2001) %>%
#   mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
#   ggplot(aes(BB_per_game, R_per_game)) + 
#   geom_point(alpha = 0.5)
# head(Teams)


smoothjazz <- Teams %>% 
  filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G)
  # ggplot(aes(AB_per_game, R_per_game)) + 
  # geom_point(alpha = 0.5)
mean(scale(smoothjazz$AB_per_game)*scale(smoothjazz$R_per_game))



rockjazz <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(error_per_game = E/G ,win_rate = W/G)  
  # ggplot(aes(win_rate, error_per_game)) +
  # geom_point(alpha = 0.5)
mean(scale(rockjazz$error_per_game)*scale(rockjazz$win_rate))



reggejazz <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(x3b_game = X3B/G ,x2b_rate = X2B/G)
  # ggplot(aes(x3b_game, x2b_rate)) +
  # geom_point(alpha = 0.5)
mean(scale(reggejazz$x2b_rate)*scale(reggejazz$x3b_game))

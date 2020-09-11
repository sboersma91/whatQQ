library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


head(bat_02)
length(bat_02)
nrow(bat_02)


#stop



jazzyjazz <- Batting %>% 
  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarize(singles_avg = mean(singles), bb_avg = mean(bb))

jazzyjazz %>% filter(playerID == "alexama02")

head(jazzyjazz)
nrow(jazzyjazz)
# stop
sum(jazzyjazz$singles_avg>.2)
sum(jazzyjazz$bb_avg>.2)
sum(jazzyjazz$bb_avg)
jazzyjazz %>% filter(playerID == "alexama02")
head(jazzyjazz)


inner_join(jazzyjazz, bat_02)












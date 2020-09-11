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


ball <- inner_join(jazzyjazz, bat_02)
p_singles <- mean(scale(ball$singles)*scale(ball$singles_avg))
p_bb <- mean(scale(ball$bb)*scale(ball$bb_avg))
p_singles
p_bb

avgsingles <-mean(female_heights$mother)
momsd <- sd(female_heights$mother)
avgdau <- mean(female_heights$daughter)
dausd <- sd(female_heights$daughter)

corr <- mean(scale(female_heights$mother)*scale(female_heights$daughter))
m <- corr*drought/monsum

#slope
corr*dausd/momsd

#intercept
b <- avgmom-m*avgdau

# inch change
momsd/dausd

avgmom+corr*(60-avgdau)/dausd*momsd
(corr^2)









library(Lahman)
library(tidyverse)
library(dslabs)
library(HistData)
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
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

ball %>% 
  ggplot(aes(singles, singles_avg)) +
  geom_point() +
  geom_smooth(method = "lm")
ball %>% 
  ggplot(aes(bb, bb_avg))+
  geom_point()+
  geom_smooth(method = "lm")

guess <- ball %>% lm(singles ~ singles_avg, data = .)
guess
s_hat <- predict(guess, se.fit = TRUE)
s_hat

wiffle <- ball %>% lm(bb ~ bb_avg, data = .)
wiffle

names(s_hat)

# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(son, father)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

ball
head(bat_02)

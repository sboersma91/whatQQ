library(Lahman)
library(tidyverse)
library(dslabs)

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
female_heights

avgmom <-mean(female_heights$mother)
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

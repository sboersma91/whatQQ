library(Lahman)
library(tidyverse)
library(dslabs)
library(HistData)
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# predicting MOTHER from daughter
female_heights

mom_avg <- mean(female_heights$mother)
mom_sd <- sd(female_heights$mother)

daughter_avg <- mean(female_heights$daughter)
daughter_sd <- sd(female_heights$daughter)

p <- mean(scale(female_heights$mother)*scale(female_heights$daughter))
m2 <- p*mom_sd/daughter_sd
m2
b <- daughter_avg-m2*mom_avg
b
mom_avg-m2*daughter_avg

# predicting Mother daughter = 69
mom_avg+ p * (69 - daughter_avg)/daughter_sd *mom_sd


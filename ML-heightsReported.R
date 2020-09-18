library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

head(reported_heights)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

x
y

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(levels = levels(y))

y_hat
dat$sex

mean(y == y_hat)
table(predicted = y_hat, actual = y)
table(y_hat, y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)
specificity
mean(dat$sex == "Female")

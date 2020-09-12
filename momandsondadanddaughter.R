library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>% 
  group_by(pair) %>% 
  summarize(n = n())


# # from book
# cors <- galton_heights %>% 
#   gather(parent, parentHeight, father:mother) %>%
#   mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
#   unite(pair, c("parent", "child")) %>% 
#   group_by(pair) %>%
#   summarize(cor = cor(parentHeight, childHeight))


galton %>% group_by(pair) %>% summarize(cor = cor(parentHeight, childHeight))

get_slope <- function(data) {
  fit <- lm(childHeight ~ parentHeight, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}
galton %>% group_by(pair) %>% do(get_slope(.))

galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight") %>% 
  select(pair, estimate, conf.low, conf.high) %>% 
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()
int<-galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight") %>% 
  select(pair, estimate, conf.low, conf.high)


# below to show the size of the conf int
int %>% mutate(innner = conf.high - conf.low)


galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(childHeight ~ parentHeight, data = .)))

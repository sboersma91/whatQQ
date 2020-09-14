library(tidyverse)
library(dslabs)
data("research_funding_rates")
research_funding_rates


sum(research_funding_rates$awards_men)


totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals
totals[[1]]


MvW <- matrix(c(totals[[1]],totals[[3]],totals[[2]],totals[[4]]), 2, 2)
rownames(MvW) <- c("Men", "Women")
colnames(MvW) <- c("Admitted", "Not_Admitted")

MvW
MvW[1]/(MvW[1,2]+MvW[1])
MvW[2]/(MvW[2,2]+MvW[2])

MvW %>% chisq.test()
MvW[1]/(MvW[1,2]+MvW[1])

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


dat %>% ggplot(aes(discipline, applications, col = gender, size = awards)) +
  geom_point()

dat %>% ggplot(aes(discipline, success, col = gender))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


dat %>% ggplot(aes(discipline, ..count.., fill = gender))+
  geom_bar()
dat %>% ggplot(aes(discipline, fill = success)) + geom_bar()
dat %>% filter(gender == "women") %>% arrange(applications)
dat %>% arrange(desc(success))

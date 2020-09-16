library(tidyverse)
library(broom)
library(Lahman)
teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

teams_small2 <- Teams_small %>% 
  mutate(avg_R = R/G, avg_HR = HR/G)


teams_small2 %>% do(tidy(lm(avg_attendance ~ avg_R, data = .)))
teams_small2 %>% do(tidy(lm(avg_attendance ~ avg_HR, data = .)))

teams_small2 %>% do(tidy(lm(avg_attendance ~ W, data = .)))
teams_small2 %>% do(tidy(lm(avg_attendance ~ yearID, data = .)))

w_corr <- teams_small2 %>% summarize(cor(W, avg_HR))
R_corr <- teams_small2 %>% summarize(cor(W,avg_R))

# W_strat <- teams_small2 %>% 
#   mutate(w_strata = round(W/10)) %>% 
#   select(yearID, teamID, G,W,L,R,AB,H,attendance,avg_attendance,avg_HR,avg_R,w_strata) %>% 
#   filter(w_strata == 5:10) LOST SOME DATA HERE!<<<<< its the damn ==!!!!!!!!!!!!! 

W_strat <- teams_small2 %>% 
  mutate(w_strata = round(W/10)) %>% 
  filter(w_strata %in% 5:10)

strat10 <- W_strat %>% filter(w_strata == 10)
strat9 <- W_strat %>% filter(w_strata == 9)
strat8 <- W_strat %>% filter(w_strata == 8)
strat7 <- W_strat %>% filter(w_strata == 7)
strat6 <- W_strat %>% filter(w_strata == 6)
strat5 <- W_strat %>% filter(w_strata == 5)

# strata_slope <- funtion(e) {
#   stratagy <- W_strat 
#   }

head(strat10)
cor(strat5$avg_attendance,strat5$avg_R) * sd(strat5$avg_attendance)/sd(strat5$avg_R)
x_attend <- strat6$avg_attendance
z_HR <- strat6$avg_HR
cor(x_attend,z_HR) * sd(z_HR)/sd(x_attend)

# y_R <- strat9$avg_R



mean(scale(x_attend)*scale(z_HR))
W_strat %>%  
  group_by(w_strata) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

W_strat %>%  
  group_by(w_strata) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G)) 
  
W_strat %>%   
  ggplot(aes(avg_R,avg_attendance, col = w_strata))+
  geom_point()+
  geom_smooth(method = "lm")
strat9 %>% 
  ggplot(aes(avg_R,avg_attendance))+
  geom_point()

head(teams_small2)
teams_small2 %>% select(avg_attendance, avg_HR, avg_R, yearID)

collins_bo <-teams_small2 %>% lm(avg_attendance ~ avg_R + avg_HR + W +yearID, data=.)
collins_bo

right1 <- predict(collins_bo, newdata = slim) 
right1
slim <- data.frame(avg_R = 5, avg_HR = 1.2, W = 80, yearID = 2002)
class(slim)
slim

jim <- Teams %>% 
  filter(yearID == 2002) %>% 
  mutate(avg_R = R/G, avg_HR = HR/G, avg_attendance = attendance/G) %>% select(avg_attendance, avg_HR, avg_R, yearID)
  
pls <- predict(jim, newdata = slim)
cor(jim$avg_attendance,teams_small2$avg_attendance )

collins_bo
cor(collins_bo, jim$avg_attendance)
collins_bo %>% pull(estimate)
fit

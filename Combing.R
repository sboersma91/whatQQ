library(tidyverse)
library(readxl)
library(readr)
library(dslabs)

url <- "https://raw.githubusercontent.com/rasbt/python-machine-learning-book/master/code/datasets/wdbc/wdbc.data"
datlamer <- read_csv(url)
download.file (url, "wdbc.data")
getwd()
yofro <- read_csv("wdbc.data")
readr(yofro, header = FALSE)
head(yofro)



co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
co2_tidy <- gather(co2_wide,month,co2,-year)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color =year))+ geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat

tmp <- gather(admissions, key, value, admitted:applicants)


unite(tmp, column_name, c(key, gender))
state   	     population
Alabama             4779736
Alaska     	         710231
Arizona    	        6392017
Delaware     	     897934
District of Columbia 601723

state <- c("Alabama", "Alaska", "Arizona", "Delaware", "District of Colombia")
pop <- c(4779736, 710231, 6391017, 8979343, 601723)

state2 <- c("Alabama", "Alaska", "Arizona", "California", "Colorado", "Connecticut")
electoral_votes <- c(9,3,11,55,9,7)

taz1 <- data.frame(state, pop)
taz2 <- data.frame(state2, electoral_votes)
x <- c("a","a")
y <- c("a","b")
df1 <- data.frame(x,y)
df2 <- data.frame(x,y)
setdiff(df1,df2)



library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>% select(playerID,nameFirst, nameLast, HR)

top_salaries <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
intersect(top_names, awards)
union(top_names, awards)
setdiff(top, awards[,1])
setequal(top_names, awards)


#the right answer length(setdiff(Awards_2016$playerID, top_names$playerID)) PLUS one more Awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)
# length(intersect(Awards_2016$playerID, top_names$playerID))
top_names
top_salaries
top
awards <- AwardsPlayers %>% filter(yearID == 2016)
awards
head(awards)

t <- awards[,1]  
p <- top[,1]
v <- t %in% p
nrow(v)
sum(v)





















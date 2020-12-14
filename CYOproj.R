# loads of comments ---------
# origin of data set https://www.kaggle.com/michaelkitchener/mbti-type-and-digital-footprints-for-reddit-users
# MBTI type and digital footprint for reddit users
# Each row contains an anonymized reddit user's MBTI personality type. 
# Each column represents how much a user posts or comments in a particular subreddit. 
# Specifically, the 'posts_ examplesubreddit' refers to how many of the users top 
# 100 posts of all time are in 'r/examplesubreddit', and 'comments_examplesubreddit' 
# refers to how many of the users most recent 100 comments are in 'r/examplesubreddit'.
# 
# This data was obtained using the PRAW (Reddit's API wrapper for python) to scrape 
# a list of reddit users who comment on the r/mbti subreddit along with their self 
# identified MBTI type (as illustrated in their flair). Then, for each user whose 
# MBTI type we are aware of, we go through their top 100 posts and newest 100 
# comments to record the frequency of their interaction in various subreddits. 
# Thus creating a user-footprint matrix.
# 
# The purpose of this data set is to see how well MBTI personality types 
# (or even just specific traits i.e. extraversion vs. introversion) 
# can be predicted on the basis of a user's subreddit interactions.
# 
# You will almost certainly need to perform some kind of dimensionality reduction 
# in order to develop an effective classification model.
# 
# Caveats:
# The MBTI type personality test is controversial and some consider it illegitimate. 
# However, both extraversion/introversion and sensing/intuition correlate strongly 
# with extraversion and openness as measured in the much more accepted big 5 model 
# of personality. As such, it might be best to focus efforts on attempting to classify 
# these traits based on the data provided.

# Opener -----
library(tidyverse)
library(caret)
reddit <- read.csv("reddit_psychometric_data.csv")

# Exploration-------
dim(reddit)
head(reddit[1:6])

# storing the types
mb <- reddit[1]

# storing the interactions
r_dat <- reddit[-1]
namez <- colnames(r_dat)


# getting an idea of number of interactions
# per person and per sub_reddit
s_col <- colSums(r_dat)
s_row <- rowSums(r_dat)

# understanding the total number of interactions per redit
max(s_col)
min(s_col)
mean(s_col)
sd(s_col)

# working with per user
max(s_row)
min(s_row)
mean(s_row)
sd(s_row)

# max(s_row)
min(s_row)
mean(s_row)
sd(s_row)

# Visualization work ------

types_table <- reddit %>% 
  group_by(mbti_type) %>% 
  summarise(total = n()) %>% 
  arrange(total)

col_df <- as.data.frame(cbind(columns = namez, totals = as.numeric(s_col)))
head(col_df)

col_df %>% 
  group_by(totals) %>% 
  summarise(total = n()) %>% 
  arrange(totals) 


# %>% 
#   filter(toals < 2500) %>% 
#   ggplot(aes(totals, total)) +
#   geom_point()



# create data frames with totals
bits <- data.frame(mbti = mb,total = s_row)
bytes <- data.frame(name = namez, total = as.numeric(s_col))
head(bits)
head(bytes)
dim(bytes)
# comment
(bits)

# graphs keep
bits %>% 
  ggplot(aes(mbti_type)) +
  geom_bar()

bytes %>% ggplot(aes(name, total)) +
  geom_point()

bytes %>% 
  filter(total < 10000) %>% 
  ggplot(aes(name, total)) +
  geom_point()

# graph testing
bytes %>% 
  filter(total < 25) %>% nrow()

# %>% 
#   ggplot(aes(name, total)) +
#   geom_point()


head(namez)

bits[max(bits$total),]
bytes[which.max(bytes$total),]
head(bytes)





# preprocessing ---------

# experimenting with size of data frame
# c = total in each column
# r = total in each row
c <- 100
r <- 25

# eliminated columns, dataframe
less_columns <- r_dat[s_col > c] 

# start this block is currently under development ----
t <- (1000 > s_col & s_col > 25)
identical(length(t), length(s_col))
sum(t)<sum(j )
# end this block-----


# list of totals 
n_row <- rowSums(less_columns)
n_reddit <- cbind(mb, less_columns, totals = n_row)
f_reddit <- n_reddit %>% filter(totals > r)
fin_redd <- f_reddit[-length(f_reddit)]


dim(f_reddit)
class(f_reddit)
# more explorations not sure where this falls, #hide it ----
max(n_row)
min(n_row)
mean(n_row)
sd(n_row)

# remake data table without used columns
cbind(mb,)


# sum(n_row > r)/nrow(reddit)
# sum(n_row>r)





# Splitting ------
fin_redd$mbti_type <- as.factor(fin_redd$mbti_type)

set.seed(2020, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(y = fin_redd$mbti_type, times = 1, p = 0.2, list = FALSE)
test <- fin_redd[test_index,]
train <- fin_redd[-test_index,]


# training ------

glmpop <- train(mbti_type ~ ., data = train, method = "glm")
ldapop <- train(mbti_type ~ ., data = train, method = "lda")
naipop <- train(mbti_type ~ ., data = train, method = "naive_bayes")
svmpop <- train(mbti_type ~ ., data = train, method = "svmLinear")

knnpop <- train(mbti_type~., data = train, method = "knn")

glopop <- train(mbti_type ~., data = train, method = "gamLoess")
mulpop <- train(mbti_type ~ ., data = train, method = "multinom")
qdapop <- train(mbti_type ~ ., data = train, method = "qda")
rfppop <- train(mbti_type ~ ., data = train, method = "rf")
adapop <- train(mbti_type ~ ., data = train, method = "adaboost")




# "final" space -----


models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")



# Opener -----
library(caret)
library(tidyverse)
reddit <- read.csv("reddit_psychometric_data.csv")

# looking to ML shlitz to work again ------
# storing the types
mb <- reddit[1]

# storing the interactions
r_dat <- reddit[-1]
namez <- colnames(r_dat)


y <- sweep(r_dat,1,rowMeans(r_dat))
pca_y <- prcomp(y)
pca_red <- prcomp(r_dat)
Sys.time()
class(pca_y)
dim(pca_y$sdev)
length(pca_y$sdev)
pca_y$sdev[1:6]
# pca_y$sdev just entries

dim(pca_y$x)
length(reddit)
nrow(reddit)


test <- cumsum(pca_y$sdev^2/sum(pca_y$sdev^2))
plot(test)




# another attempt from web



# lists the pc's in order = pca_y$rotation also same  = (pca_y$x)
max(pca_y$center)

dim(r_dat)
dim(y)
dim(pca_y$x)
prcomp()


# this is what was in the ML stuff udemy

ppp <- preProcess(x = train[-1], method = "pca")

training_set <- predict(ppp, train)
view(training_set)
















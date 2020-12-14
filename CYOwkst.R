# Opener -----
# parameter testing/ able to run whole script from scratch

library(tidyverse)
library(caret)
reddit <- read.csv("reddit_psychometric_data.csv")

# Exploration-------

# storing the types
mb <- reddit[1]

# storing the interactions
r_dat <- reddit[-1]
namez <- colnames(r_dat)


# getting an idea of number of interactions
# per person and per sub_reddit
s_col <- colSums(r_dat)
s_row <- rowSums(r_dat)


# Visualization work ------

# create data frames with totals
bits <- data.frame(mbti = mb,total = s_row)
bytes <- data.frame(name = namez, total = as.numeric(s_col))


# preprocessing ---------

# experimenting with size of data frame
# c = total in each column
# r = total in each row
c <- 100
r <- 55

# eliminated columns, dataframe
less_columns <- r_dat[s_col > c] 


# list of totals 
n_row <- rowSums(less_columns)

# look at n_row

n_reddit <- cbind(mb, less_columns, totals = n_row)
f_reddit <- n_reddit %>% filter(totals > r)
fin_redd <- f_reddit[-length(f_reddit)]


# Splitting ------
fin_redd$mbti_type <- as.factor(fin_redd$mbti_type)

set.seed(2020, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(y = fin_redd$mbti_type, times = 1, p = 0.2, list = FALSE)
test <- fin_redd[test_index,]
train <- fin_redd[-test_index,]


# training ------
r
Sys.time()

# had warnings - 27
ldapop <- train(mbti_type ~ ., data = train, method = "lda")
Sys.time()

naipop <- train(mbti_type ~ ., data = train, method = "naive_bayes")
Sys.time()

# had warnings - 14
svmpop <- train(mbti_type ~ ., data = train, method = "svmLinear")
Sys.time()

knnpop <- train(mbti_type~., data = train, method = "knn")
Sys.time()

# attempt again - installed packages
glopop <- train(mbti_type ~., data = train, method = "gamLoess")
# Sys.time()

r
ldapop
naipop
svmpop
knnpop
glopop


reddit %>% 
  group_by(mbti_type) %>% 
  summarise(total = n()) %>% 
  arrange(total)


# "final" space -----


models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda",  "adaboost")



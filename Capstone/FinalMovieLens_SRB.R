# This first set of code is the prompt code to start analyzing.
# R version 4.0.2 was used in analysis.
library(tidyverse)
library(caret)
library(data.table)
#above will be deleted
# prompt-------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Creating edx_train, edx_test  -------------------------------

# Creating a train and test set from the initial edx test set for model building

set.seed(31, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index2]
temp2 <- edx[test_index2]

#ensure userId and movieId are in both train and test sets
edx_test <- temp2 %>% 
  semi_join(edx_train, by = "movieId") %>% 
  semi_join(edx_train, by = "userId") 

removed2 <- anti_join(temp2, edx_test)
edx_train <- rbind(edx_train, removed2)

rm(test_index2, temp2, removed2)

# data exploration insert-----------------------


# Mutate Movie Year & Timestamp as Date ---------------
edx2 <- edx %>% 
  mutate(year = str_extract((str_extract(edx$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx$timestamp)))

validation2 <- validation %>% 
  mutate(year = str_extract((str_extract(validation$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(validation$timestamp)))

edx_train2 <- edx_train %>% 
  mutate(year = str_extract((str_extract(edx_train$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx_train$timestamp)))

edx_test2 <- edx_test %>% 
  mutate(year = str_extract((str_extract(edx_test$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx_test$timestamp)))



# Error calculated with RMSE (also found in caret package)
RMSE1 <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2)
  )}

# Mean   -------------------

# first predict by the average rating
avg_rating <- mean(edx_train$rating)

# create a table to check progress
result <- tibble(Method = "Avg", RMSE = RMSE(avg_rating, edx_test$rating))
result

# Movie Bias -----------------
m_bias <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(m_bias = mean(rating - avg_rating))


m_bias_pred <- edx_test %>% 
  left_join(m_bias, by = "movieId") %>% 
  mutate(pred = avg_rating + m_bias) %>% 
  pull(pred)

result <- bind_rows(result, tibble(Method = "Avg + M_bias", 
                                   RMSE = RMSE(m_bias_pred, edx_test$rating)))
result


# User Bias ---------------------
u_bias <- edx_train %>% 
  left_join(m_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(u_bias = mean(rating - avg_rating - m_bias))

u_bias_pred <- edx_test %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId") %>% 
  mutate(pred = avg_rating + m_bias + u_bias) %>% 
  pull(pred)

result <- bind_rows(result, tibble(Method = "Avg + M_bias + U_bias",
                                   RMSE = RMSE(u_bias_pred, edx_test$rating)))
result

# Genre Bias ------------------

g_bias <- edx_train %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias,  by = "userId") %>%
  group_by(genres) %>%
  summarise(g_bias = mean(rating - avg_rating - m_bias - u_bias))

g_bias_pred <- edx_test %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  mutate(pred = avg_rating + m_bias + u_bias + g_bias) %>%
  pull(pred)

RMSE(edx_test$rating, g_bias_pred)

result <- bind_rows(result, tibble(Method = "Avg + M_bias + U_bias + G_bias",
                                   RMSE = RMSE(g_bias_pred, edx_test$rating)))

result

# year released -----------------

y_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>%
  left_join(g_bias, by = "genres")  %>%
  group_by(year) %>%
  summarise(y_bias = mean(rating - avg_rating - m_bias - u_bias - g_bias))


y_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year") %>% 
  mutate(pred = avg_rating + m_bias + u_bias + g_bias + y_bias) %>%
  pull(pred)

RMSE(y_bias_pred, edx_test2$rating)

result <- bind_rows(result, tibble(Method = "Avg + M_bias + U_bias + G_bias + Y_bias",
                                   RMSE = RMSE(y_bias_pred, edx_test2$rating)))

result

# Date Rated Bias ---------------------

d_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  group_by(date) %>% 
  summarise(d_bias = mean(rating - avg_rating - m_bias - u_bias - g_bias - y_bias))

d_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>% 
  mutate(pred = avg_rating + m_bias + u_bias + g_bias + y_bias + d_bias) %>%
  pull(pred)

RMSE(d_bias_pred, edx_test2$rating)

result <- bind_rows(result, tibble(Method = "Avg + M_bias + U_bias + G_bias + Y_bias + D_bias",
                                   RMSE = RMSE(d_bias_pred, edx_test2$rating)))


result %>% ggplot(aes(x= Method, y = RMSE)) + 
  geom_point()+
  geom_text(aes(label=RMSE))


# regularization Function----------------------
# one time with mean, movie, user, genre, year released, #date rated


lambda_values <- seq(0, 10, 0.25)


regularization <- sapply(lambda_values, function(lambda_best2){
  
  
  mean_rating <- mean(edx_train$rating)
  
  movie_bias <- edx_train2 %>% 
    group_by(movieId) %>% 
    summarize(movie_bias = sum(rating - mean_edx)/(n()+lambda_best2))
  
  user_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(user_bias = sum(rating - mean_edx - movie_bias)/(n()+lambda_best2))
  
  genre_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId") %>%
    group_by(genres) %>% 
    summarise(genre_bias = sum(rating - mean_edx - movie_bias - user_bias)/(n()+lambda_best2))
  
  year_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId")  %>%
    left_join(genre_bias, by = "genres")  %>%
    group_by(year) %>%
    summarise(year_bias = sum(rating - mean_edx - movie_bias - user_bias - genre_bias)/(n()+lambda_best2))
  
  date_bias <- edx_train2 %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId")  %>%
    left_join(genre_bias, by = "genres")  %>%
    left_join(year_bias, by = "year")    %>%
    group_by(date)  %>%
    summarise(date_bias = sum(rating - mean_edx - movie_bias - user_bias - genre_bias - year_bias)/(n()+lambda_best2))
  
  predict_ratings_edx <- edx_test2 %>% 
    left_join(movie_bias, by = "movieId")%>% 
    left_join(user_bias,  by = "userId") %>%
    left_join(genre_bias, by = "genres") %>% 
    left_join(year_bias,  by = "year")   %>% 
    left_join(date_bias,  by = "date")   %>% 
    mutate(pred = mean_edx + movie_bias + user_bias + genre_bias + year_bias + date_bias) %>% 
    pull(pred)
  
  RMSE(predict_ratings_edx, edx_test$rating)
})




lambda_best <- lambda_values[which.min(regularization)]
lambda_best

result <- bind_rows(result, tibble(Method = "Regularization",
                                   RMSE = min(regularization)))
# data exploration/piddling-----------
library(ggplot2)
tibble(lambda = lambda_values, RMSE = regularization) %>% 
  ggplot(aes(x = lambda, y = RMSE))+
  geom_point()
lambda_values[which.min(regularization)]


# Final Regularization Model --------------------

lambda_best

mean_edx <- mean(edx$rating)

movie_bias <- edx2 %>% 
  group_by(movieId) %>% 
  summarize(movie_bias = sum(rating - mean_edx)/(n()+lambda_best))

user_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(user_bias = sum(rating - mean_edx - movie_bias)/(n()+lambda_best))

genre_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias,  by = "userId") %>%
  group_by(genres) %>% 
  summarise(genre_bias = sum(rating - mean_edx - movie_bias - user_bias)/(n()+lambda_best))

year_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias,  by = "userId")  %>%
  left_join(genre_bias, by = "genres")  %>%
  group_by(year) %>%
  summarise(year_bias = 
              sum(rating - mean_edx - movie_bias - user_bias - genre_bias)/(n()+lambda_best))

predict_ratings_edx <- validation2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>%
  left_join(genre_bias, by = "genres") %>% 
  left_join(year_bias,  by = "year") %>% 
  mutate(pred = mean_edx + movie_bias + user_bias + genre_bias + year_bias) %>% 
  pull(pred)

error_edx_all <- RMSE(predict_ratings_edx, validation$rating)

error_edx_all

result <- bind_rows(result, tibble(Method = "Final Error: Regularization + Bias",
                                   RMSE = RMSE(predict_ratings_edx, validation$rating)))

# -------------- checking points of final ERROR -----------

# 10 pts
error_edx_all < 0.89999

# 15 pts
error_edx_all < 0.86549

# 20 pts
error_edx_all< 0.86499

# 25 pts
error_edx_all < 0.86490
# 25pts of regularization
min(regularization) < 0.86490
# Test > train error
error_edx_all > min(regularization)


error_edx_all









# with the Reco system, it makes no difference for the error value

# ---------------Reco systems-----------------
# first is the train and test sets

if(!require(recosystem))
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

set.seed(2020, sample.kind = "Rounding")
reco_train_data <- with(edx_train, data_memory(user_index = userId,
                                               item_index = movieId,
                                               rating = rating))
reco_test_data <- with(edx_test, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating = rating))
reco <- Reco()
tune <- reco$tune(reco_train_data)



reco_tune <- reco$train(reco_train_data, opts = tune$min)

reco_predict <- reco$predict(reco_test_data, out_memory())

reco_rmse <-RMSE(reco_predict, edx_test$rating)
reco_rmse

result <- bind_rows(result, tibble(Method = "Matrix Factorization, RecoSystem",
                                   RMSE = reco_rmse))


# Final RecoSystem - Matrix Factorization ------------------------



set.seed(2020, sample.kind = "Rounding")
reco_train_data_edx <- with(edx, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating = rating))
reco_test_data_edx <- with(validation, data_memory(user_index = userId,
                                                   item_index = movieId,
                                                   rating = rating))

reco_edx <- Reco()


tune_edx <- reco_edx$tune(reco_train_data_edx, opts = (nthread = 4))
# nthread did not speed up the process :/
tune_edx

reco_edx$train(reco_train_data_edx, opts = tune_edx$min)


reco_predict_edx <- reco_edx$predict(reco_test_data_edx, out_memory())
reco_predict_edx

reco_rmse_edx <- RMSE(reco_predict_edx, validation$rating)
reco_rmse_edx

result <- bind_rows(result, tibble(Method = "Final Error: Matrix Factorization, RecoSystem",
                                   RMSE = reco_rmse_edx))


result
# end space ------------------------------

















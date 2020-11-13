
edx2 <- edx %>% mutate(year = str_extract(edx$title, "\\d{4}"))
validation2 <- validation %>% mutate(year = str_extract(validation$title, "\\d{4}"))
edx_train2 <- edx_train %>% mutate(year = str_extract(edx_train$title, "\\d{4}"))
edx_test2 <- edx_test %>% mutate(year = str_extract(edx_test$title, "\\d{4}"))


head(edx2)
head(validation2)


# ----------year released ---------

year_test <- edx_train %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>%
  left_join(g_bias, by = "genres")  %>%
  mutate(year = str_extract(edx_train$title, "\\d{4}")) %>% 
  group_by(genres) %>%
  summarise(g_bias = mean(rating - avg_rating - m_bias - u_bias))


identical(year_test, year_bias)


year_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>%
  left_join(g_bias, by = "genres")  %>%
  group_by(genres) %>%
  summarise(g_bias = mean(rating - avg_rating - m_bias - u_bias))






g_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  mutate(pred = avg_rating + m_bias + u_bias + g_bias) %>%
  pull(pred)

RMSE(edx_test$rating, g_bias_pred)



























# ----------------------------------this is where changing code starts ---------------------------------------- 

lambda_values <- seq(0, 10, 0.25)

regularization <- sapply(lambda_values, function(lambda){
  
  mean_rating <- mean(edx_train$rating)
  
  movie_bias <- edx_train %>% 
    group_by(movieId) %>% 
    summarize(movie_bias = sum(rating - mean_rating)/(n()+lambda))
  
  user_bias <- edx_train %>% 
    left_join(movie_bias, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(user_bias = sum(rating - movie_bias - mean_rating)/(n()+lambda))
  
  predict_ratings <- edx_test %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias, by = "userId") %>% 
    mutate(pred = mean_rating + movie_bias + user_bias) %>% 
    pull(pred)
  
  RMSE(predict_ratings, edx_test$rating)
})

result

# 10 pts
min(regularization) < 0.89999
# 15 pts
min(regularization) < 0.86549

# 20 pts
min(regularization) < 0.86499

# 25 pts
min(regularization) < 0.86490

lambda_values[which.min(regularization)]

# tibble(Lambda = lambda_values, RMSE = regularization) %>% ggplot(aes(x = Lambda, y = RMSE)) + geom_point()

lambda_best <- lambda_values[which.min(regularization)]

mean_rating <- mean(edx_train$rating)

movie_bias <- edx_train %>% 
  group_by(movieId) %>% 
  summarize(movie_bias = sum(rating - mean_rating)/(n()+lambda_best))

user_bias <- edx_train %>% 
  left_join(movie_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(user_bias = sum(rating - movie_bias - mean_rating)/(n()+lambda_best))

predict_ratings <- edx_test %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId") %>% 
  mutate(pred = mean_rating + movie_bias + user_bias) %>% 
  pull(pred)


result <- bind_rows(result, tibble(Method = "Regularized Movie and User bias",
                                   RMSE = RMSE(predict_ratings, edx_test$rating)))

result


# Regularization using, movies, users, years, and genres

lambda_values <- seq(0, 10, 0.25)

regularization2 <- sapply(lambda_values, function(lambda){
  

  mean_rating <- mean(edx_train$rating)
  
  movie_bias <- edx_train %>% 
    group_by(movieId) %>% 
    summarize(movie_bias = sum(rating - mean_rating)/(n()+lambda))
  
  user_bias <- edx_train %>% 
    left_join(movie_bias, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(user_bias = sum(rating - mean_rating - movie_bias)/(n()+lambda))

  genre_bias <- edx_train %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId") %>%
    group_by(genres) %>% 
    summarise(genre_bias = sum(rating - mean_rating - movie_bias - user_bias)/(n()+lambda))
  
  # time_bias <- edx_train %>% 
  #   left_join(movie_bias, by = "movieId") %>% 
  #   left_join(user_bias,  by = "userId") %>% 
  #   group_by(timestamp) %>% 
  #   summarise(time_bias = sum(rating - mean_rating - movie_bias - user_bias)/(n()+lambda))
    
  predict_ratings <- edx_test %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId") %>% 
    # left_join(year_bias,  by = "year") %>%
    left_join(genre_bias, by = "genres") %>% 
    mutate(pred = mean_rating + movie_bias + user_bias + genre_bias) %>% # year_bias + pulled out
    pull(pred)
  
  RMSE(predict_ratings, edx_test$rating)
})



head(edx)
identical(min(regularization2), min(regularization))

min(regularization2) - min(regularization)



# 10 pts
min(regularization2) < 0.89999
# 15 pts
min(regularization2) < 0.86549

# 20 pts
min(regularization2) < 0.86499

# 25 pts
min(regularization2) < 0.86490

lambda_values[which.min(regularization2)]

# tibble(Lambda = lambda_values, RMSE = regularization) %>% ggplot(aes(x = Lambda, y = RMSE)) + geom_point()

lambda_best2 <- lambda_values[which.min(regularization2)]


avg_rating



# recosystem

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


result








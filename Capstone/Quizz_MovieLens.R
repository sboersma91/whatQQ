# final project Movie Lens.
# Set up code

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

# if using R 4.0 or later:
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
# This ends the given code now it is up to the rest of us 
# pls note the global environment was reset and therefore shouldn't need to be done again.


head(validation)
head(edx)
nrow(edx)


ThreeStarRating <- edx %>% filter(rating == 3)
nrow(ThreeStarRating)
edx %>% filter(rating == is.na())
nas <- is.na(edx$rating)
sum(nas)
titles <- edx %>% select(title = title)
unique_titles <- unique(titles)
identical(titles, unique_titles)
nrow(unique_titles )
titles %>% group_by(title)
head(edx)
movie_id <- edx %>% select(movieId)
nrow(unique(movie_id))
users <- edx %>% select(userId)
nrow(unique(users))
drama <- edx %>% filter(genres %in% "Drama")
head(drama)


comdedy <- edx %>% filter(genres %in% "Comedy")
head(comdedy)

?str_detect
sum(str_detect(edx$genres, "Romance"))


lst <- c(sum(str_detect(edx$title, "Jurassic Park")),

sum(str_detect(edx$title, "Pulp Fiction")),

# edx$title[str_detect(edx$title, "Shawshank")]

sum(str_detect(edx$title, "Speed 2: Cruise Control")),

sum(str_detect(edx$title, "Forrest Gump")))

max(lst)
edx %>% filter("Shawshank Redemption, The (1994)" %in% title ) %>% select(title)

edx %>% group_by(title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

lst










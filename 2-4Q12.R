library(tidyverse)
library(ggplot2)
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]



# the work above is the needed code to set this all up correctly
head(train)


cutoff_L <- seq(range(train[,3])[1],range(train[,3])[2], by = 0.1)
cutoff_W <- seq(range(train[,4])[1], range(train[,4])[2],by = 0.1)

acc_W <- map_dbl(cutoff_W, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})


acc_W # accuracy of each number in the range
max(acc_W)
best_cut_W <- cutoff_W[which.max(acc_W)]

acc_L <- map_dbl(cutoff_L, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

acc_L #accuracy of each number in the range
max(acc_L)
best_cut_L <- cutoff_L[which.max(acc_L)]

y_hat_Fin <- ifelse(test$Petal.Length > best_cut_L | test$Petal.Width > best_cut_W, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
head(y_hat_Fin)


mean(y_hat_Fin == test$Species) #I did it. Larry didn't prevail today.... atm.
# stop()

# the "actual" answer 
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

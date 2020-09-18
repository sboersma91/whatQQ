library(dslabs)
library(tidyverse)
library(ggplot2)
library(caret)
data(iris)

head(iris)


iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(2, sample.kind="Rounding") 
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test<- iris[test_index,]
train <- iris[-test_index,]

n <- iris %>% filter(Species == "versicolor")
length(n)
n
nrow(n)
# versiolor, virginica

cutoff <- seq(1,9, .1)
acc <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
acc
max(acc)



# ---------------------------------------- start of their code... qq
# question 8
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


# question 9
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]

y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)

# question 10
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
sapply(predictions,which.max)	

#-------------------------------------- question 11
plot(iris,pch=21,bg=iris$Species)

clue <- function (x){
  rangedValues <- seq(range(x)[1], range(x)[2])
  sapply(rangedValues, function(i){
    
  })
}

  
  foo <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
      y_hat <- ifelse(x>i,'virginica','versicolor')
      mean(y_hat==train$Species)
    })
  }
predictions <- apply(train[,-5],2,foo)
predictions

sapply(predictions,max)	  



?apply


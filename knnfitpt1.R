library(caret)
library(tidyverse)
library(dslabs)
data("heights")
y <- heights$sex

set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

k <- seq(1,101,3)

z <- sapply(k, function(x){
  knn_fit <- knn3(sex ~ height, data = train_set, k=x)
  y_hat <- predict(knn_fit, test_set, type = "class")
  F_meas(data = y_hat, reference = test_set$sex)
})
max(z)

# this below is the same as above. 
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]

train_set <- heights[-test_index, ]
k <- seq(1, 101, 3)

F_1 <- sapply(k, function(x){
  
  knn_fit <- knn3(sex ~ height, data = train_set, k = x)
  
  y_hat <- predict(knn_fit, test_set, type = "class")
  
  F_meas(data = y_hat, reference = test_set$sex)
  
})



max(F_1)

k[which(F_1 == max(F_1), arr.ind = TRUE)]

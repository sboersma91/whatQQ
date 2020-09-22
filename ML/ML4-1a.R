library(caret)
library(tidyverse)
library(dslabs)
data(tissue_gene_expression)



set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later
str(tissue_gene_expression)

test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)

train_set_x <- tissue_gene_expression$x[-test_index,]
train_set_y <- tissue_gene_expression$y[-test_index]

train_set <- list(x = train_set_x, y = train_set_y)
# attempted to use data.frame ehre but that dropped a whole half of data


test_set_x <- tissue_gene_expression$x[test_index,]
test_set_y <- tissue_gene_expression$y[test_index]

length(test_set$x)
length(test_set$y)


test_set <- list(x = test_set_x, y = test_set_y)


k <- seq(1, 11, 2)

sapply(k, function(x){
  knn_fit <- knn3(y ~ x, data = train_set, k = x)
  y_hat <- predict(knn_fit, test_set, type = "class")
  cm <- confusionMatrix(data = y_hat, reference = test_set$y)$overall["Accuracy"]
  
})


# answer
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})
library(caret)
library(dslabs)
library(tidyverse)

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
names(fits) <- models

# kernlab, naive_byes, ababoost,fastAdaboost,

fits[[1]]
length(mnist_27$test$y)       

i <- seq(1, 10)

pred_models <- sapply(i, function(i){predict(fits[i], mnist_27$test,simplify="matrix")
  })

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
# sapply(pred, function(z){
#   confusionMatrix(as.factor(z), as.factor(mnist_27$test$y))
# })
pred

# $overall["Accuracy"]

length(mnist_27$test$y)
length(models)
  
# o <- sapply(i, function(x){
#   predict(fits[x], mnist_27$test$y, simplify = "matrix")
# })
    
m <- sapply(pred_models, function(x){
  r<-confusionMatrix(as.factor(x), as.factor(mnist_27$test$y))$overall["Accuracy"]
  mean(r)
})
m
mean(m)

acc <- colMeans(pred == mnist_27$test$y)
acc
mean(acc)


rowAcc <- rowMeans(pred == 7)
vote <- ifelse(rowAcc > 0.5, 7, 2)
confusionMatrix(data = factor(vote), reference = factor(mnist_27$test$y))$overall[["Accuracy"]]

sum(m>= .81)
which(m>= .81)

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

accura <- sapply(i, function(x){
  which(max(fits[[x]]$results$Accuracy)>=.8)
})
mean(accura)
accura
fits[[10]]$results$Accuracy
fits[10]$results$Accuracy
fits[10]
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
rm <- c(lda, smvLinear, multinom)
fits_2 <- fits


# take it from the top #yoloswaggernever

set.seed(1, sample.kind = "Rounding")
i2 <- seq(1,7)

models_2 <- c("glm", "2", "naive_bayes", "4", "knn", "gamLoess", "7", "qda", "rf", "adaboost")
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
fits_3 <- fits
names(fits_3) <- models_2

pred_models2 <- sapply(i2, function(i){predict(fits_3[i], mnist_27$test,simplify="matrix")
})

pred2 <- sapply(fits_3, function(object) 
  predict(object, newdata = mnist_27$test))


votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

ind <- acc > mean(y_hat == mnist_27$test$y)
sum(ind)
models[ind]

ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)









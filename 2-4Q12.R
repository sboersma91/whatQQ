


cutoff <- seq(1,9, .1)
acc <- function(x){
 rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
 y_hat <- ifelse(train$Petal.Length > x & train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
}
acc
max(acc)
predictions <- apply(train[,-5],2,acc)
sapply(predictions,max)	
    
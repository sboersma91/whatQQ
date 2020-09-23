library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

head(indexes)
sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

x <- sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)
y <- rnorm(100, 0, 1)

quantile(y, .075)

set.seed(1, sample.kind = "Rounding")
flare <- replicate(10000,{
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(flare)
sd(flare)


set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
l<- createResample(y, 10000)


k <- sapply(l, function(ind){
  quantile(y[ind], 0.75)
})

mean(k)
sd(k)
# you can apply an index to in a sapply function. see above.

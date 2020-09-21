library(dslabs)
library(tidyverse)
library(caret)

set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind="Rounding")
mkr <- replicate(100, {
  test_index <- createDataPartition(dat$y, times=1, p=.5, list = FALSE)
  train_set <- dat[-test_index,]
  test_set <- dat[test_index,]
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(mkr)
sd(mkr)

set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)

sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  mkr <- replicate(100, {
    test_index <- createDataPartition(dat$y, times=1, p=.5, list = FALSE)
    train_set <- dat[-test_index,]
    test_set <- dat[test_index,]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(avg = mean(mkr), sd = sd(mkr))
}) 
  
sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))  
  laker <- replicate(100,{
    test_index <- createDataPartition(dat$y, times = 1, p = .5, list = FALSE)
    train_set <- dat[-test_index,]
    test_set <- dat[test_index,]
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
c(mean(laker), sd(laker))  
}) 

# Question number 6 boyiii
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(dat$y, times=1, p=.5, list = FALSE)
train_set <- dat[-test_index,]
test_set <- dat[test_index,]
fit <- lm(y ~ x_1 + x_2,  data = train_set)
y_hat <- predict(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y))+ geom_density()
mu <- seq(0, 3, len=25)

twofive <- make_data(mu)
twofive
glom <- glm(y ~ x, data = twofive$train, family = "binomial")
phat <- predict(glom, newdata = twofive$test, type = "response")
phat
twofive %>% ggplot(aes(twofive,))

make_data(25)


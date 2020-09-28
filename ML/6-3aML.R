library(tidyverse)
library(dslabs)

options(digits = 7)
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))


# end setup code
top10 <- schools %>% top_n(10, corrected) %>% arrange(desc(corrected))
top10
median(schools$size)
median(top10$size)

bottom10 <- schools %>% top_n(-10, quality) %>% arrange(quality)
median(bottom10$size)
schools %>% 
  ggplot(aes(score, size))+
  geom_point(alpha = 0.3) +
  geom_point(data = top10, aes(score, size), color = 'red')+
  geom_point(data = bottom10, aes(score,size), color = "blue")




overall <- mean(sapply(scores, mean))
overall

alpha <- 135

schools <-
  schools %>% 
  mutate(corrected = overall + .$size*(.$score - overall)/(.$size + alpha))


sapply(alpha, function(x){
  snookie<-schools %>% 
      mutate(corrected = overall + .$size*(.$score - overall)/(.$size + alpha))
  
})

sapply(alpha, function(x){
  overall + schools$size*(schools$score - overall)/(schools$size + alpha)
})

RMSES_schools <- sapply(alpha, function(a){
  sapply(scores, function(b) {
    results <- overall + sum(b-overall)/(length(b)+a)
    sqrt((1/1000)*sum((schools$quality-results))^2)
  })})

alpha[which.min(colSums(RMSES_schools))]

RMSES_schools

alphas <- seq(10,250)


rmses <- sapply(alphas, function(a){
  score_reg <- sapply(scores, function(x){overall + sum(x-overall)/(length(x)+a)})
  schoolsWreg <- schools %>% mutate(score_reg = score_reg)
  RMSE(schoolsWreg$quality, schoolsWreg$score_reg)
})

which.min(rmses)
alpha[126]
# not 10, 250
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
plot(alphas, rmse)
alphas[which.min(rmse)]


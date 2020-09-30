options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
library(gam)
data(brca)



x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

sd(x_scaled[,1])

median(x_scaled[,1])

d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y=="B")
mean(test_y=="B")

# machine learning start
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding")
k <- kmeans(train_x, centers = 2)

y_hat_kmeans1 <- predict_kmeans(test_x, k)
y_hat_kmeans <- ifelse(y_hat_kmeans1 == 1, "B", "M")
mean(y_hat_kmeans == test_y)


index2 <- which(test_y == "B")
index3 <- which(test_y == "M")
mean(y_hat_kmeans[index2] == test_y[index2])
mean(y_hat_kmeans[index3] == test_y[index3])

pred<-train(x=train_x, y= train_y, method = "glm" )
jk <- predict(pred, test_x)
jk
mean(jk == test_y)

lydia <- train(x= train_x, y = train_y, method= "lda")
quinna <- train(x=train_x, y = train_y, method= "qda")
p_lydia <- predict(lydia, test_x)
p_quinna <- predict(quinna, test_x)
mean(p_lydia == test_y)
mean(p_quinna == test_y)

set.seed(5, sample.kind = "Rounding")
lois <- train(x = train_x, y = train_y, method = "gamLoess")
plois <- predict(lois, test_x)
mean(plois == test_y)

set.seed(7, sample.kind = "Rounding")
kj <- seq(3,21,2)
kpop <- train(x= train_x, y = train_y, method = "knn", tuneGrid = data.frame(k =seq(3,21,2))) 
kpp <- predict(kpop, test_x)
mean(kpp == test_y)
kpop

set.seed(9, sample.kind = "Rounding")
furry <- train(x = train_x, y = train_y, method= "rf", tuneGrid =data.frame(mtry = c(3, 5, 7, 9)), importance = TRUE)
furry
furpr <- predict(furry, test_x)
furpr
length(furpr)
varImp(furry)
varImp(furpr)
mean(furpr == test_y)

c()

# these are the predictions as B's and M's
# kmeans = y_hat_kmeans and now it is cap
# glm = jk
# lday = p_lydia
# qda = p_quinna
# loess = plois
# knn = kpp
# rf = furrypr
as.factor(cap)
class(cap)
class(kpp)

?matrix
muzack <- c(cap, jk, p_lydia, p_quinna, plois, kpp, furpr)
muzack1 <- ifelse(muzack == 1, "B","M")
# c("kmeans","glm","lda","qda","loess","knn","rf")
muzack1
cap
length(kpp)
chez <- matrix(muzack, nrow = 115, ncol = 7)
chez[,1]
p_lydia
chez[,1]
identical(chez[,1], cap)
tail(kpp)
# 1=B , 2= M
cap <- ifelse(cap == "B",1,2) #made the kmeans from b's to 1's and 2's
cap

# order no larry
muzack <- c(cap, jk, p_lydia, p_quinna, plois, kpp, furpr)
muzack #has all 1's and 2's
chez <- matrix(muzack, nrow = 115, ncol = 7)
mail <- rowMeans(chez == 1)
rob <- ifelse(mail>.5, 1, 2)
robby <- ifelse(rob == 1, "B", "M")

mail[which((robby == test_y)== FALSE)]

votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")
mean(y_hat == mnist_27$test$y)

head(rob)
length(test_y)
length(rob)
dim(chez)
mail
length(mail
       )
length(rob)
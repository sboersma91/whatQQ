library(tidyverse)
library(caret)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

head(x_subset)

fit <- train(x_subset, y, method = "glm")
fit$results

# did these steps already... WOOw. 
# install.packages("BiocManager")
# BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
tt
head(tt)
pvals <- tt$p.value
ind <- which(pvals<=0.01)
ind
# where the idk??? begins

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,ind]
zed <- x[,ind]
head(zed)
zed_fit <- train(zed, y, method = "glm")
zed_fit$results$Accuracy

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

kxk <- seq(1,7,2)

library(dslabs)
data("tissue_gene_expression")
head(tissue_gene_expression)
train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))

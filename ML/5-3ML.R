library(dslabs)
library(tidyverse)
library(caret)
library(rpart)
data("tissue_gene_expression")
set.seed(1991, sample.kind = "Rounding")

lit <- with(tissue_gene_expression, train(x,y, 
                                   method = "rpart", 
                                   tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))
                                   ))
# the above using the "with" function doesn't need the tissues as a data frame
# cp = seq() is how you name the collumn
# the train function doesn't use the ~ as the lm one does

ggplot(lit)
lit$finalModel
plot(lit$finalModel)
text(lit$finalModel)

set.seed(1991, sample.kind= "Rounding")
lit2 <- with(tissue_gene_expression, train(x,y, 
                                          method = "rpart", 
                                          control = rpart.control(minsplit = 0),
                                          tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))
))
confusionMatrix(lit2)

plot(lit2$finalModel)
text(lit2$finalModel)

set.seed(1991, sample.kind= "Rounding")
lit3 <- with(tissue_gene_expression, train(x,y, 
                                           method = "rf", 
                                           nodesize = 1,
                                           tuneGrid = data.frame(mtry = seq(50, 200, 25))
))
fit_rpart <- lit3
confusionMatrix(lit3)
tree_terms <- as.character(unique(lit$finalModel$frame$var[!(lit$finalModel$frame$var == "<leaf>")]))
tree_terms
as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
varImp(lit)
varImp(lit2)
varImp(lit3)

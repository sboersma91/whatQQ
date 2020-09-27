library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(dslabs)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42, sample.kind = "Rounding")
tnic_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train <- titanic_clean[-tnic_index,]
test <- titanic_clean[tnic_index,]

set.seed(1, sample.kind = "Rounding")
x <- as.numeric(train$Fare)
y <- as.numeric(train$Survived)
train %>% train(y ~ x, method = "lda")
class(y)
class(x)
head(train$Fare)

# question 7
set.seed(1, sample.kind = "Rounding")
f_lda <- train(Survived ~ Fare, method = "lda", data = train)
pred_lda <- predict(f_lda, test) 
confusionMatrix(pred_lda, reference = test$Survived)
set.seed(1, sample.kind = "Rounding")
f_qda <- train(Survived ~ Fare, method = "qda", data = train)
pred_qda <- predict(f_qda, test)
confusionMatrix(pred_qda, reference = test$Survived)

# question 8
set.seed(1, sample.kind = "Rounding")
f_glm_a <- train(Survived~Age, data = train, method = "glm")
pred_glm_A <- predict(f_glm_a, test)
confusionMatrix(pred_glm_A, reference = test$Survived)

set.seed(1, sample.kind = "Rounding")
f_glm_b <- train(Survived ~ c(Age,  Pclass,  Sex,  Fare), data = train, method = "glm")
pred_glm_B <- predict(f_glm_b, test)
confusionMatrix(pred_glm_B, test$Survived)


head(train)

set.seed(1, sample.kind = "Rounding")
f_glm_c <- train(Survived ~., data = train, method = "glm")
pred_glm_c <- predict(f_glm_c, test)
confusionMatrix(pred_glm_c, test$Survived)


# set.seed(1, sample.kind = "Rounding") 
# train_glm_all <- train(Survived ~ ., method = "glm", data = train)
# glm_all_preds <- predict(train_glm_all, test)
# mean(glm_all_preds == test$Survived)

set.seed(6, sample.kind = "Rounding")
f_knn1 <- train(Survived ~ ., method = "knn", data = train, tuneGrid = data.frame(k = seq(3,51,2)))
pred_knn1 <- predict(f_knn1, test)
mean(pred_knn1 == test$Survived)

ggplot(f_knn1)
f_knn1$bestTune

set.seed(8, sample.kind = "Rounding")
cross <- train(Survived ~., method = "knn", data = titanic_clean, tuneGrid = data.frame(k = seq(3,51,2)))
predcross <- predict(cross, test)
mean(predcross  == test$Survived)

# queston10

set.seed(8, sample.kind = "Rounding")
troll <- trainControl(method = "cv", number = 10)

train(Survived~., 
      method = "knn", 
      data = titanic_clean, 
      tuneGrid = data.frame(k = seq(3,51,2)))

guess <- train(Survived ~ ., 
               method = "knn", 
               tuneGrid = data.frame(k = seq(3,51,2)), 
               trControl = trainControl(method = "cv", number = 10),
               metric = "Accuracy",
               data = titanic_clean )
guess             
# tried attempts on question 10 = .693, .709, 719, 721

set.seed(8, sample.kind="Rounding")
train_control <- trainControl(method="cv", number=10)
fit_knn_cross <- train(Survived~., data=train, method="knn", trControl=train_control, tuneGrid = data.frame(k = seq(3,51,2)))

pred_knn_cross <- predict(fit_knn_cross, test)
confusionMatrix(pred_knn_cross, test$Survived)$overall["Accuracy"]

# question 11

set.seed(10, sample.kind = "Rounding")
f_rpart <- train(Survived~., 
                 method = "rpart", 
                 tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                 data = train)
f_rpart2 <- predict(f_rpart, test)
mean(f_rpart2 == test$Survived)
f_rpart$bestTune

plot(f_rpart$finalModel)
text(f_rpart$finalModel)

# Final Question PLS
set.seed(14, sample.kind = "Rounding")
f_rf <- train(Survived ~.,
              method = "rf",
              tuneGrid = data.frame(mtry = seq(1:7)),
              data = train,
              ntree = 100
              )
pred_rf <- predict(f_rf, test)
mean(pred_rf == test$Survived)

#guessed 0.801, 0.849, (actual answer is 0.844, the tuneGid vs tunGrid #LOL)

varImp(f_rf)



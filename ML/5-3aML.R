library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
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

nrow(train)
nrow(test)
mean(train$Survived == 1)

set.seed(3, sample.kind = "Rounding")
y_hat_1 <- sample(c(0,1), nrow(test), replace =TRUE)
mean(y_hat_1 == test$Survived)

train %>% group_by(Sex) %>% summarise(Winners = mean(Survived == "1"))

train %>% filter(Sex =="male") %>% summarise(mean(Survived == "1"))
train %>% filter(Sex == "female") %>% summarise(mean(Survived == "1"))
class(train$Survived)

j <- ifelse(test$Sex == "female", 1, 0)
mean(j == test$Survived)

train %>% group_by(Pclass) %>% summarize(mean(Survived == "1"))
p <- ifelse(test$Pclass =="1", 1,0)
mean(p == test$Survived)

train %>% group_by(Sex, Pclass) %>% summarise(mean(Survived == "1"))


qw <- ifelse(test$Sex == "female" & (test$Pclass == "2" | test$Pclass == "1"), 1, 0)
mean(qw == test$Survived)

identical(w,qw)
confusionMatrix(as.factor(j), reference = test$Survived)
confusionMatrix(as.factor(p), reference = test$Survived)
confusionMatrix(as.factor(qw), reference = test$Survived)

F_meas(as.factor(j), reference = test$Survived)
F_meas(as.factor(p), reference = test$Survived)
F_meas(as.factor(qw), reference = test$Survived)

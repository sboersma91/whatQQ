library(dslabs)
library(tidyverse)
library(titanic)
library(ggplot2)
options(digits = 3)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Age,Sex) %>%
  ggplot(aes(Age, y = ..density.., fill = Sex))+
  geom_density(alpha = 0.2)



titanic %>%  
  filter(!is.na(Age)) %>%
  ggplot(aes(Age ,y = ..density.., fill=Survived )) + 
  geom_density(alpha=0.2)

titanic %>%
  group_by(Survived,Age) %>%
  ggplot(aes(Sex ,y = ..density.., fill=Sex )) + 
  geom_density(alpha=0.2)

titanic %>% ggplot(aes(Age ,y = ..density.., fill=Survived )) + geom_density(alpha=0.2)
#My code 
titanic %>% 
  filter(Fare > 0)%>%
  group_by(Fare,Survived) %>%
  ggplot(aes(Fare, Survived, fill = Survived)) +
  geom_boxplot()+
  geom_point(position = "jitter")+ #not sure if this was 
  scale_x_continuous(trans = "log2")
#answer's code vs whats above ^^
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare, fill= Survived)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)


#question6 
titanic %>%                                   #filter(Pclass == 1) %>%
  #group_by(Pclass,Survived) %>%
  ggplot(aes(Pclass, ..count.., fill = Survived))+
  geom_bar()
  
#correct answer I danced around it lul
# barplot of passenger class filled by survival
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar() +
  ylab("Proportion")
# barplot of passenger class filled by survival with position_fill
titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
# Barplot of survival filled by passenger class with position_fill
titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill()) +
  ylab("Proportion")
z1 <- titanic %>% 
     filter(Pclass == 1 & Survived == 0) #%>%
    #pull(Survived)
nrow(z1)
z2 <- titanic %>% 
      filter(Pclass == 2 & Survived == 1)
nrow(z2)
z3 <- titanic %>% 
      filter(Pclass == 3)

  
titanic %>%  
  filter(!is.na(Age)) %>%
  ggplot(aes(Age ,y = ..count.., fill=Survived )) + 
  geom_density(alpha=0.2) +
  facet_grid(Sex ~ Pclass)
  
  
  
  
  














  
  



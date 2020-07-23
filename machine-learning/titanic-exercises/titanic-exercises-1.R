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

#set the seed to 42
set.seed(42)

#create a 20% data partition based on the Survived column
#set.seed(42)
index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[index,]
train_set <- titanic_clean[-index,]
nrow(train_set)

nrow(train_set)
nrow(test_set)

#What proportion of individuals in the training set survived?
mean(train_set$Survived == 1)

#set seed to 3
set.seed(3)

#For each individual in the test set, randomly guess whether that person 
#survived or not by sampling from the vector c(0,1)
#What is the accuracy of this method?
guess <- sample(c(0, 1), replace = TRUE, size = nrow(test_set))
mean(guess == test_set$Survived)

#What proportion of training set females survived? What proportion of 
#training set males survived?
female_only <- train_set %>% filter(Sex == "female")
male_only <- train_set %>% filter(Sex == "male")
mean(female_only$Survived == 1)
mean(male_only$Survived == 1)

#if the survival rate for a sex is over 0.5, predict survival for all 
#individuals of that sex, and predict death if the survival rate for a sex is 
#under 0.5. What is the accuracy?
sex_predict <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_predict == test_set$Survived)

#In the training set, which class(es) (Pclass) of passengers were more likely 
#to survive than die?
train_set %>% group_by(Pclass) %>% summarize(Survived = mean(Survived == 1))

#Predict survival using passenger class on the test set: predict survival if 
#the survival rate for a class is over 0.5, otherwise predict death.
#What is the accuracy of this class-based prediction method on the test set?
class_predict <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_predict == test_set$Survived)

#Use the training set to group passengers by both sex and passenger class.
train_set %>% group_by(Sex, Pclass) %>% 
  summarize(Survived = mean(Survived == 1))

#Predict survival using both sex and passenger class on the test set. 
#Predict survival if the survival rate for a sex/class combination is over 0.5, 
#otherwise predict death. What is the accuracy of this sex- and class-based 
#prediction method on the test set?
class_and_sex_prediction <- 
  ifelse(test_set$Sex == "female" & test_set$Pclass %in% c(1, 2), 1, 0)
mean(class_and_sex_prediction == test_set$Survived)

#Use the confusionMatrix() function to create confusion matrices for the sex 
#model, class model, and combined sex and class model
confusionMatrix(data = factor(sex_predict), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_predict), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_and_sex_prediction), reference = factor(test_set$Survived))

#Which model has the highest F1 score?
F_meas(data = factor(sex_predict), reference = factor(test_set$Survived))
F_meas(data = factor(class_predict), reference = factor(test_set$Survived))
F_meas(data = factor(class_and_sex_prediction), reference = factor(test_set$Survived))
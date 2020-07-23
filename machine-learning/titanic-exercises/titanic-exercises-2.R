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

#Set the seed to 1
set.seed(1)

#Train a model using linear discriminant analysis using fare as the only 
#predictor
lda_model <- train(Survived ~ Fare, data = train_set, method = "lda")
lda_prediction <-predict(lda_model, test_set)
mean(lda_prediction == test_set$Survived)

#Set the seed to 1. Train a logistic regression model using fare as the only 
#predictor.
set.seed(1)
qda_model <- train(Survived ~ Fare, data = train_set, method = "qda")
qda_prediction <- predict(qda_model, test_set)
mean(qda_prediction == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the glm method using 
#age as the only predictor.
set.seed(1)
glm_model <- train(Survived ~ Age, data = train_set, method = "glm")
glm_pred <- predict(glm_model, test_set)
mean(glm_pred == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the glm method using 
#four predictors: sex, class, fare, and age.
set.seed(1)
glm_model2 <- train(Survived ~ Age + Sex + Pclass + Fare, data = train_set, method = "glm")
glm_pred2 <- predict(glm_model2, test_set)
mean(glm_pred2 == test_set$Survived)

#Set the seed to 1. Train a logistic regression model with the glm method 
#using all predictors. Ignore warnings about rank-deficient fit.
set.seed(1)
glm_model3 <- train(Survived ~ ., data = train_set, method = "glm")
glm_pred3 <- predict(glm_model3, test_set)
mean(glm_pred3 == test_set$Survived)

#Set the seed to 6. Train a kNN model on the training set. 
#Try tuning with k = seq(3, 51, 2).
set.seed(6)
knn_model <- train(Survived ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 51, 2)))
knn_model$bestTune

plot(knn_model)

#What is the accuracy of the kNN model on the test set?
knn_pred <- predict(knn_model, test_set)
mean(knn_pred == test_set$Survived)

#Set the seed to 8 and train a new kNN model. Instead of the default training 
#control, use 10-fold cross-validation where each partition consists of 10% 
#of the total.
#Try tuning with k = seq(3, 51, 2). What is the optimal value of k using 
#cross-validation?
set.seed(8)
knn_cv_model <- train(Survived ~ .,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
knn_cv_model$bestTune

#What is the accuracy on the test set using the cross-validated kNN model?
knn_cv_pred <- predict(knn_cv_model, test_set)
mean(knn_cv_pred ==  test_set$Survived)

#Set the seed to 10. Use caret to train a decision tree with the rpart method. 
#Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
#What is the optimal value of the complexity parameter (cp)?
set.seed(10)
rpart_model <- train(Survived ~ .,
                     method = "rpart",
                     data = train_set,
                     tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
rpart_model$bestTune

#What is the accuracy of the decision tree model on the test set?
rpart_pred <- predict(rpart_model, test_set)
mean(rpart_pred == test_set$Survived)

#Inspect the final model and plot the decision tree.
#Which variables are used in the decision tree?
rpart_model$finalModel
plot(rpart_model$finalModel, margin = 0.1)
text(rpart_model$finalModel)

#Set the seed to 14. Use the caret train() function with the rf method to 
#train a random forest. Test values of mtry ranging from 1 to 7. 
#Set ntree to 100.
set.seed(14)
rf_model <- train(Survived ~ .,
                  method = "rf", 
                  data = train_set,
                  tuneGrid = data.frame(mtry = seq(1, 7, 1)),
                  ntree = 100)

#What mtry value maximizes accuracy?
rf_model$bestTune

#What is the accuracy of the random forest model on the test set?
rf_pred <- predict(rf_model, test_set)
mean(rf_pred == test_set$Survived)

#Use varImp() on the random forest model object to determine the 
#importance of various predictors to the random forest model.
varImp(rf_model)
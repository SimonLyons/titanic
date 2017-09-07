
install.packages("caret")

require(dplyr)
require(caret)
require(lattice)
require(tidyr)

# Set seed for reproducibility
set.seed(1977)

summary(orig_train)
# Subset training set down into complete Age data
train_age_complete <- orig_train %>% 
  filter(!is.na(Age))
summary(train_age_complete)
str(train_age_complete)
str(orig_train)

# Break complete age subset into training and test sets
samp <- createDataPartition(y = train_age_complete$PassengerId, p = 0.7, list = FALSE)
samp_age_train <- train_age_complete[samp, ]
samp_age_test <- train_age_complete[-samp, ]

str(samp_age_train)
summary(samp_age_train)
View(samp_age_train)

# Build age predictor using pls method
age_model <- train(Age ~., data = samp_age_train, method = "pls")
summary(age_model)
# Predict age using new model
samp_age_test$Age2 <- round(predict(age_model, newdata = samp_age_test), 0)
samp_age_test$Delta2 <- abs(samp_age_test$Age - samp_age_test$Age2)
View(samp_age_test %>% select(Age, Age2, Delta2))
sum2 <- summary(samp_age_test$Delta2)
# Use confusion matrix to evaluate accuracy
confusionMatrix(samp_age_test$Age, samp_age_test$Age2)


# Build age predictor using random forrest method
age_model <- train(Age ~., data = samp_age_train, method = "rf")
summary(age_model)
# Predict age using new model
samp_age_test$Age3 <- round(predict(age_model, newdata = samp_age_test), 0)
samp_age_test$Delta3 <- abs(samp_age_test$Age - samp_age_test$Age3)
View(samp_age_test %>% select(Age, Age3, Delta3))
sum3 <- summary(samp_age_test$Delta3)

# Build age predictor using rpart decision tree method
age_model <- train(Age ~., data = samp_age_train, method = "rpart")
summary(age_model)
# Predict age using new model
samp_age_test$Age4 <- round(predict(age_model, newdata = samp_age_test), 0)
samp_age_test$Delta4 <- abs(samp_age_test$Age - samp_age_test$Age4)
View(samp_age_test %>% select(Age, Age4, Delta4))
sum4 <- summary(samp_age_test$Delta4)

# Build age predictor using repeated cross validation method
age_model <- train(Age ~., data = samp_age_train, method = "blackboost")   # Not currently working
summary(age_model)
# Predict age using new model
samp_age_test$Age5 <- round(predict(age_model, newdata = samp_age_test), 0)
samp_age_test$Delta5 <- abs(samp_age_test$Age - samp_age_test$Age5)
View(samp_age_test %>% select(Age, Age5, Delta5))
sum5 <- summary(samp_age_test$Delta5)

res <- rbind(sum2, sum3, sum4, sum5)
View(res)
# Methods for use in caret's 'train' function
method = "rpart", method = "repeatedcv", method = "boot", method = "cv"
# Use confusion Matrix to inspect accuracy of results
confusionMatrix(test$pred.leaf.rf, test$Class)


# Predict Age using linear regression
samp_age_train_red <- samp_age_train %>% 
  mutate(malefemale = 1) %>% 
  spread(Sex, malefemale, fill = 0) %>% 
  # mutate("Age_lm" = (Age)^2) %>% 
  select(Age, Pclass, SibSp, Parch, Fare, male, female)
View(samp_age_train_red)


splom(samp_age_train_red)
mean(samp_age_train[samp_age_train$Sex == "male", "Age"])

# Let's examine the average age of passengers at each Embarkation point
samp_age_train %>% group_by(Embarked) %>% summarise("Embarked_Mean_Age" = mean(Age))
# We discover the average age spread across the three sites in within a year. Not significant.

# Let's look at the average Fare for each ticket class
samp_age_train %>% group_by(Pclass) %>% summarise("Fare_mean_per_class" = mean(Fare))


age_model <- train(data = samp_age_train_red, Age ~ (Pclass)+ SibSp + male, method = "lm")
summary(age_model)



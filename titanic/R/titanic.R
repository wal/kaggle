# https://www.kaggle.com/c/titanic
# http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/

library(tidyverse)

train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

str(train)
table(train$Survived)
round(prop.table(table(train$Survived)) * 100, 0)

# Submission 1 - Everyone Dies

submit <- test %>%
  mutate(Survived = 0) %>%
  select(PassengerId, Survived)


table(submit$Survived)

write.csv(submit, 'results/submit_1-everyone_dies.csv', row.names = FALSE)


# Submission 2 - Impact of Sex on survival

summary(train$Sex)
prop.table(table(train$Sex, train$Survived)) * 100
prop.table(table(train$Sex, train$Survived), 1) * 100
prop.table(table(train$Sex, train$Survived), 2) * 100


submit <- test %>%
  mutate(Survived = ifelse(Sex == 'male', 0, 1))

write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_2-men_die.csv',
  row.names = FALSE
)


# Submission 3 - Impact of Age on survival
summary(train$Age)
train$Child <- train$Age < 18
str(train)
aggregate(data = train, Survived ~ Child + Sex, FUN = sum)
aggregate(
  data = train,
  Survived ~ Child + Sex,
  FUN = function(x) {
    mean(x)
  }
)


# Submission #4 - Impact of Fare and pClass

summary(train$Fare)
fare_category <- function(fare) {
  if (fare > 30) {
    return('30+')
  } else if (fare > 20) {
    return('20-30')
  } else if (fare > 10) {
    return('10-20')
  } else {
    return('<10')
  }
}

train <- train %>%
  rowwise() %>%
  mutate(Fare2 = fare_category(Fare))

train$Fare2 <- as.factor(train$Fare2)

summary(train$Fare2)
summary(train$Pclass)

aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = mean)

survived <- function(sex, pclass, fare) {
  if (sex == 'female') {
    if (pclass == 3 && fare >= 20) {
      return(1)
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}

submit <- test %>%
  rowwise() %>%
  mutate(Survived = survived(Sex, Pclass, Fare))


write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_3-sex-fare-class.csv',
  row.names = FALSE
)

# Submission 4 - Decision Tree
# A glass-box model, after the model has found the patterns in the data you can see exactly what
# decisions will be made for unseen data that you want to predict

round(prop.table(table(train$Survived)) * 100, 0)

library(rpart)
fit <-
  rpart(
    Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
    data = train,
    method = 'class'
  )

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")

submit <-
  data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_4-decision_tree.csv',
  row.names = FALSE
)

# Feature Engineering - chopping, and combining different attributes
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

test$Survived <- NA
combined <- rbind(train, test)
combined$Name <- as.character(combined$Name)
combined$Name[1]


# Refactor with stringr/tidyr
combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined$Title <- sub(' ', '', combined$Title)

table(combined$Title)

combined$Title[combined$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

combined$Title <- factor(combined$Title)

combined$FamilySize <- combined$SibSp + combined$Parch + 1
combined$Surname <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined$FamilyID <- paste(as.character(combined$FamilySize), combined$Surname, sep="")

combined$FamilyID[combined$FamilySize <= 2] <- 'Small'
table(combined$FamilyID)

famIDs <- data.frame(table(combined$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combined$FamilyID[combined$FamilyID %in% famIDs$Var1] <- 'Small'
combined$FamilyID <- factor(combined$FamilyID)

train <- combined[1:891,]
test <- combined[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")

fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")

submit <-
  data.frame(PassengerId = test$PassengerId, Survived = Prediction)

write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_5-decision_tree_feature_engineering.csv',
  row.names = FALSE
)



# Submission 5 - Random Forests
# Grow a lot of different models, and let their outcomes be averaged or voted across the group.

# Bagging 
sample(1:10, replace = TRUE)
sample(1:10, replace = FALSE)


summary(combined$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combined[!is.na(combined$Age),], 
                method="anova")
combined$Age[is.na(combined$Age)] <- predict(Agefit, combined[is.na(combined$Age),])


summary(combined)

which(combined$Embarked == '')
combined$Embarked[c(62,830)] = "S"
combined$Embarked <- factor(combined$Embarked)
which(is.na(combined$Fare))
combined$Fare[1044] <- median(combined$Fare, na.rm=TRUE)

str(combined)


combined$FamilyID2 <- combined$FamilyID
combined$FamilyID2 <- as.character(combined$FamilyID2)
combined$FamilyID2[combined$FamilySize <= 3] <- 'Small'
combined$FamilyID2 <- factor(combined$FamilyID2)

summary(combined)

library(randomForest)
set.seed(415)


train <- combined[1:891,]
test <- combined[892:1309,]


fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_6-random_forest.csv',
  row.names = FALSE
)


# Submission 6 - Conditional Inference Trees

library(party)

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FamilySize + FamilyID,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))


Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(
  submit %>% select(PassengerId, Survived),
  'results/submit_7-conditional_inference_tree.csv',
  row.names = FALSE
)
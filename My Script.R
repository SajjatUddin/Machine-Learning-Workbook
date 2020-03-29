setwd("~/Machine Learning Work Book/Titanic")

train <- read.csv("~/Machine Learning Work Book/Titanic/train.csv")

train

View(train)

test <- read.csv("~/Machine Learning Work Book/Titanic/test.csv")

test

View(test)

table(train$Survived)

prop.table(table(train$Survived))

prop.table(table(train$Survived))*100

barplot(table(train$Survived), xlab="Survived", ylab="People", main="Train Data Survival")

test$Survived <- rep(0, 418)

prediction1 <- data.frame(PassengerId = test$PassengerId,
Survived = test$Survived)

write.csv (prediction1, file = "1stPrediction.csv",
row.names = FALSE)

library(Amelia)

missmap(train, main="Titanic Training Data - Missings Map",
col =c("yellow","black"), legend=FALSE)

# barplot for all the variables 

# How many survived?
barplot(table(train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")

# Passengers travelling in different classes
barplot(table(train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")

# How many survived gender-wise?
barplot(table(train$Sex), main="Sex (gender)", col="darkviolet")

# Age distribution in the Titanic
hist(train$Age, main="Age", xlab = NULL, col="brown")

# How in group people (sibling+spouse) were traveling?
barplot(table(train$SibSp), main="SibSp (siblings + spouse aboard)", col="darkblue")

# How parents and children were traveling? 
barplot(table(train$Parch), main="Parch (parents + kids aboard)", col="gray50")

# What was the fair most people paid for Titanic?
hist(train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, col="darkgreen")

# Where most people Embarked?
barplot(table(train$Embarked),
        names= c("Missing", "Cherbourg", "Queenstown", "Southampton"),
        main="Port of Embarkation")

summary (train$Sex)

prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived), 1)*100

barplot(table(train$Sex), xlab = "Passenger",
        ylab = "People", main = "Train Data Passenger")

test$Survived <-0

test$Survived [test$Sex == 'female'] <-1

Prediction2 <- data.frame(PassengerId = test$PassengerId,
        Survived = test$Survived)

write.csv(Prediction2, file = "2ndPrediction.csv", row.names = FALSE)

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

view(train)


table(train$Sex, train$Survived)

summary(train$Sex)

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN = sum)

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN = length)

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) 

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)*100}) 


test$Survived <- 0

test$Survived[test$Sex == 'female'] <- 1

test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


prediction3 <- data.frame(PassengerId = test$PassengerId,
                  Survived = test$Survived)

write.csv (prediction3, file = "3rdPrediction.csv", row.names=FALSE)

install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

mytree1 <- rpart(Survived ~ Sex, data = train, method = "class")

fancyRpartPlot(mytree1)

mytree2 <- rpart(Survived ~ Age + Pclass, data = train, method = "class")

fancyRpartPlot(mytree2)

mytree3 <- rpart(Survived ~ Pclass + Sex + Age + SibSp
                + Parch + Fare + Embarked, data = train, method = "class")

plot(mytree3)

text(mytree3)

fancyRpartPlot(mytree3)

Prediction4th <- predict(mytree3, test, type = "class")

Prediction4 <- data.frame(PassengerId = test$PassengerId,
                          Survived = Prediction4th)

write.csv(Prediction4, file = "4thPrediction.csv", row.names = FALSE)



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


test$Survived <- NA

combined_set <- rbind(train, test)

combined_set$Name <- as.character(combined_set$Name)

combined_set$Child[combined_set$Age < 14] <- 'Child'

combined_set$Child[combined_set$Age >= 14] <- 'Adult'

table(combined_set$Child, combined_set$Survived)

combined_set$Child <- factor(combined_set$Child)

combined_set$Mother <- 'Not Mother'

combined_set$Mother[combined_set$Sex == 'female' & 
        combined_set$Parch > 0 & combined_set$Age > 18] <- 'Mother'


table(combined_set$Mother, combined_set$Survived)

combined_set$Mother <- factor


# What's in a name, again?
combined_set$Name[1]


# Find the indexes for the tile piece of the name
strsplit(combined_set$Name[1], split='[,.]')
strsplit(combined_set$Name[1], split='[,.]')[[1]]
strsplit(combined_set$Name[1], split='[,.]')[[1]][2]


# Engineered variable: Title
combined_set$Title <- strsplit(combined_set$Name, split='[,.]')[[1]][2]  # Won't work!
combined_set$Title <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combined_set$Title <- sub(' ', '', combined_set$Title)


# Inspect new feature
table(combined_set$Title)


# Combined_set New Small Title Groups
combined_set$Title[combined_set$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combined_set$Title[combined_set$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined_set$Title[combined_set$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


# Convert to a factor
combined_set$Title <- factor(combined_set$Title)


# Adding Mother variable
combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' 
        & combined_set$Parch > 0 & combined_set$Age > 18
        & combined_set$Title != 'Miss'] <- 'Mother'


# Cabin
combined_set$Cabin[1:20]
combined_set$Cabin <- as.character(combined_set$Cabin)
strsplit(combined_set$Cabin[2], NULL)[[1]]
combined_set$Deck<-factor(sapply(combined_set$Cabin, function(x) strsplit(x, NULL)[[1]][1])) 

# Fare Price

combined_set$Fare_type[combined_set$Fare<50]<-"low"
combined_set$Fare_type[combined_set$Fare>50 & combined_set$Fare<=100]<-"med1"
combined_set$Fare_type[combined_set$Fare>100 & combined_set$Fare<=150]<-"med2"
combined_set$Fare_type[combined_set$Fare>150 & combined_set$Fare<=500]<-"high"
combined_set$Fare_type[combined_set$Fare>500]<-"vhigh"

aggregate(Survived~Fare_type, data=combined_set,mean)

# Engineered variable: Family size
                                 
combined_set$FamilySize <- combined_set$SibSp + combined_set$Parch + 1
                                 
                                 
# Engineered variable: Family

combined_set$Surname <- sapply(combined_set$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combined_set$FamilyID <- paste(as.character(combined_set$FamilySize), combined_set$Surname, sep="")

combined_set$FamilyID[combined_set$FamilySize <= 2] <- 'Small'

combined_set$FamilySizeGroup[combined_set$FamilySize == 1] <- 'single'
combined_set$FamilySizeGroup[combined_set$FamilySize < 5 & combined_set$FamilySize > 1] <- 'Smaller'
combined_set$FamilySizeGroup[combined_set$FamilySize > 4] <- 'large'

mosaicplot(table(combined_set$FamilySizeGroup, combined_set$Survived), main='Survival affected by Family Size ', shade=TRUE)


table(combined_set$FamilyID)
table(combined_set$FamilySizeGroup)

# Convert to a factor

combined_set$FamilyID <- factor(combined_set$FamilyID)
combined_set$FamilySizeGroup <- factor(combined_set$FamilySizeGroup)

# Split back into test and train sets

train <- combined_set[1:891,]
test <- combined_set[892:1309,]


# Build a new tree with our new features

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + Mother + SibSp + Parch + Deck + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")

fancyRpartPlot(fit)

Prediction5th <- predict(fit, test, type = "class")

Prediction5 <- data.frame(PassengerId = test$PassengerId,
                          Survived = Prediction5th)

write.csv(Prediction5, file = "5thPrediction.csv", row.names = FALSE)
                                 
# Data pre-Processing & Cleaning
# Fill in Age NAs

summary(combined_set$Age)

FillAge <- rpart(Age ~ Pclass + Mother + FamilySize + Sex + SibSp
                 + Parch + Deck + Fare + Embarked + Title + FamilyID
                 + FamilySizeGroup + FamilySize, 
                 data=combined_set[!is.na(combined_set$Age),], method="anova")

combined_set$Age[is.na(combined_set$Age)] <- predict(FillAge, combined_set[is.na(combined_set$Age),])

summary(combined_set$Age)

summary(combined_set)

summary(combined_set$Embarked)

which(combined_set$Embarked == '')

combined_set$Embarked[c(62,830)] = "S"

combined_set$Embarked <- factor(combined_set$Embarked)

summary(combined_set$Fare)

which(is.na(combined_set$Fare))

combined_set$Fare[1044] <- median(combined_set$Fare, na.rm = TRUE)


library('mice')
library('lattice')

md.pattern(combined_set)



# New factor for new technique , only allowed <32 levels, so reduce number

combined_set$FamilyID2 <- combined_set$FamilyID

# Convert back to string

combined_set$FamilyID2 <- as.character(combined_set$FamilyID2)
combined_set$FamilyID2[combined_set$FamilySize <= 3] <- 'Small'

# And convert back to factor

combined_set$FamilyID2 <- factor(combined_set$FamilyID2)

#once again for both the variable
# Mother

combined_set$Mother <- 'Not Mother'
combined_set$Mother[combined_set$Sex == 'female' & combined_set$Parch > 0 & combined_set$Age > 18] <- 'Mother'
combined_set$Mother <- factor(combined_set$Mother)

# Child

combined_set$Child[combined_set$Age < 14] <- 'Child'
combined_set$Child[combined_set$Age >= 14] <- 'Adult'
combined_set$Child <- factor(combined_set$Child)

# Check what else might be missing

summary(combined_set)

# Split back into test and train sets

train <- combined_set[1:891,]
test <- combined_set[892:1309,]

# Build a new tree with our new features

dtree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilySizeGroup + FamilyID + FamilyID2,
               data=train, method="class")

# Now let's make a prediction and write a submission file

Prediction6th <- predict(dtree, test, type = "class")
Prediction6 <- data.frame(PassengerId = test$PassengerId, Survived = Prediction6th)
write.csv(Prediction6, file = "6thprediction.csv", row.names = FALSE)

                                 


                                 
                                 
                                 

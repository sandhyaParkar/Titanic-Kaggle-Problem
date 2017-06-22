# Sandhya Parkar
# Date: 18/06/2017

#Setting the working directory and importing the data sets
setwd("D:/EdWisor Notes/Projects/Kaggle")

library(readr)

train <- read_csv("D:/EdWisor Notes/Projects/Kaggle/train.csv")
View(train)

test <- read_csv("D:/EdWisor Notes/Projects/Kaggle/test.csv")
View(test)


str(train)
table(train$Survived)
prop.table(table(train$Survived))

# we will assume that all the passangers in test data died
test$Survived <- rep(0, 418)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit , file = "submission.csv", row.names = FALSE)

# we will look at the summary of the variable Sex
summary(train$Sex)
table(train$Sex)

# compare male and females that survived
table(train$Survived, train$Sex)

#let's see the proportion row wise
prop.table(table(train$Sex, train$Survived),1)

# with above output we can conclude that majority of female are survived 
# so updating our Survived column in test data
test$Survived <- 0
test$Survived[test$Sex== "female"] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit , file = "submission.csv", row.names = FALSE)

# now let us move towards age variable
summary(train$Age)

# Feature engineering
# let's create a new variable child where age is less than 18
train$child <- 0
train$child[train$Age < 18] <- 1

# to see proportion of survival for different subsets
aggregate(Survived ~ child + Sex , data= train ,FUN = sum)

# to know total number of people in each subset
aggregate(Survived ~ child + Sex , data= train, FUN = length)

# to find the proportion of survived
aggregate(Survived ~ child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

# Let's do binning for fare variable
train$Fare2 <- "30+"
train$Fare2[train$Fare <30 & train$Fare >= 20] <- "20-30"
train$Fare2[train$Fare < 20 & train$Fare >=10] <- "10-20"
train$Fare2[train$Fare < 10] <- "<10"

# let's run aggregate wrt pclass sex and fare2 variables
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x){sum(x)/length(x)})

# let's make new prediction based on pclass fare and sex
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex =="female" & test$Pclass == 3 & test$Fare >= 20] <- 0

# create output file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submission.csv" ,row.names = FALSE)

# Modelling using decision tree
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch +Fare +  Embarked , data = train, method = "class")
plot(fit)
text(fit)

# installing some packages for better visualization
#install.packages("rpart.plot")
#install.packages("rattle")
#install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# try to show our plot a little nicer
fancyRpartPlot(fit)

# making prediction on test data
Prediction <- predict(fit, test, type = "class")

# writing to csv
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file ="submission.csv", row.names = FALSE)

#?rpart.control
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 2, cp = 0))
fancyRpartPlot(fit)
 Prediction <- predict(fit, test, type = "class")
 
# writing to csv
 submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
 write.csv(submit, file="submission.csv", row.names = FALSE)
 
#let's do some feature engineering
 train$Name[5]
 
# row binding two data sets together
 test$Survived <- NA
 train <- train[ , 1:12]
 data <- rbind(train , test)
 
 # changing the data type
 data$Name <- as.character(data$Name)
 
 data$Name[1]
 
 #spiltting the string by , and .
# strsplit(data$Name[1], split = '[,.]')
 #strsplit(data$Name[1], split = '[,.]')[[1]]
 #strsplit(data$Name[1], split = '[,.]')[[1]][2]
 #data$title <- strsplit(data$Name, split = '[,.]')[[1]][2]
 data$title <- sapply(data$Name, FUN = function(x){ strsplit(x,split = '[,.]')[[1]][2]} )
 data$title <- sub(' ','',data$title)
 table(data$title)
 
#reducing the factor level
 data$title[data$title %in% c('Mme',  'Mlle')] <- 'Mlle'
 table(data$title)
 data$title[data$title %in% c('Capt','Major','Don','Sir')] <- 'Sir'
 data$title[data$title %in% c('Dona','Jonkheer','Lady','theCountess')] <- 'Lady'

# again converting these to factor
 data$title <- factor(data$title)
 data$FamilySize <- data$SibSp + data$Parch + 1

# extracting the surname
 data$Surname <- sapply(data$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][1]})

# combining FamilySize with Surname
 data$FamilyID <- paste(as.character(data$FamilySize), data$Surname, sep = " ")
 
data$FamilyID[data$FamilySize <= 2] <- 'small'
table(data$FamilyID)
famId <- data.frame(table(data$FamilyID) )
famId <- famId[famId$Freq <= 2,] 
data$FamilyID[data$FamilyID %in% famId$Var1] <- 'Small'
data$FamilyID <-factor(data$FamilyID)

# splitting the data into train and test
train <- data[1:891, ]
test <- data[892: 1309, ]

# model building
fit <- rpart(Survived ~ Pclass + Sex + Age + FamilySize + FamilyID + Embarked + Parch + SibSp + Fare + title , data= train, method = "class")
Prediction <- predict(fit, test,type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="submission.csv", row.names = FALSE)

# let's work on age variable
summary(data$Age)
# predicting the values that are NA in age
age_fit <- rpart(Age ~ Pclass + Sex + Embarked + Fare + SibSp + Parch + title +FamilySize, data=data[!is.na (data$Age), ], method = "anova")
data$Age[is.na(data$Age)] <- predict(age_fit, data[is.na(data$Age), ])
summary(data)

# jump out to vars which are out of attention yet
summary(data$Embarked)
table(data$Embarked)
data[is.na(data$Embarked), ]

# we replace these 2 observation with 'S' being majority
data$Embarked[c(62, 830)]= "S"
data$Embarked <- factor(data$Embarked)
summary(data$Fare)
data[is.na(data$Fare), ]
data$Fare[1044] <- median(data$Fare, na.rm = TRUE)

data$FamilyID2 <- data$FamilyID
data.FamilyID2 <- as.character(data$FamilyID2)
data$FamilyID2[data$FamilySize <= 3] <- 'Small'
data$FamilyID2 <-factor(data$FamilyID2)
data$Sex <- factor(data$Sex)

# now building model Random Forest
install.packages("randomForest")
library(randomForest)

# splitting the data into train and test
train <- data[1:891, ]
test <- data[892: 1309, ]

# setting the seed
set.seed(123)
train$Sex <- factor(train$Sex)


fit <- randomForest(as.factor(Survived) ~ Pclass + Age + Sex + SibSp + Parch + FamilyID2 + Embarked + FamilySize + title , data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)

Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file="submission.csv" , row.names = FALSE)

# Build condition inference tree Random Forest
install.packages("party")
library(party)

set.seed(123)

fit <- cforest(as.factor(Survived) ~ Pclass + Age + Sex + Fare + FamilyID + FamilySize + Embarked + SibSp + Parch + title, data = train, controls = cforest_unbiased( ntree=2000, mtry=3) )
Prediction <- predict(fit, test, OOB = TRUE, type="response")

#Prediction
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file= "submission.csv", row.names = FALSE)

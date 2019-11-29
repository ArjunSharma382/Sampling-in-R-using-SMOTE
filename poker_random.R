# The intent of the project is to learn poker rules by applying different learning algorithms
# Approaches used:
# Decision tree
# Random forest
# Support Vector Machine
# Extreme Gradient Boosting
# Nueral Net

#######################################################################################################
####DECISION TREE & RANDOM FOREST####
#######################################################################################################
# Define working directory
setwd("C:/Users/arjun_0ozfyct/Documents/Clustring")

#######################################################################################################
#######################################################################################################
# Load the dataset
data1<-data.frame(read.csv("poker-hand-training-true.csv", header = TRUE, stringsAsFactors = TRUE))

#Add column names for each attribute
colnames(data1)<-c("Suit_of_card_1","Rank_of_card_1","Suit_of_card_2","Rank_of_card_2","Suit_of_card_3","Rank_of_card_3","Suit_of_card_4","Rank_of_card_4","Suit_of_card_5","Rank_of_card_5","Poker_Hands")
head(data1)
str(data1)
summary(data1)

# Change data format to factors
data3 <- data1
data3$Suit_of_card_1 <- as.factor(data3$Suit_of_card_1)
data3$Suit_of_card_2 <- as.factor(data3$Suit_of_card_2)
data3$Suit_of_card_3 <- as.factor(data3$Suit_of_card_3)
data3$Suit_of_card_4 <- as.factor(data3$Suit_of_card_4)
data3$Suit_of_card_5 <- as.factor(data3$Suit_of_card_5)
data3$Rank_of_card_1 <- as.factor(data3$Rank_of_card_1)
data3$Rank_of_card_2 <- as.factor(data3$Rank_of_card_2)
data3$Rank_of_card_3 <- as.factor(data3$Rank_of_card_3)
data3$Rank_of_card_4 <- as.factor(data3$Rank_of_card_4)
data3$Rank_of_card_5 <- as.factor(data3$Rank_of_card_5)
data3$Poker_Hands <- as.factor(data3$Poker_Hands)
str(data3)

#######################################################################################################
#######################################################################################################
# Install & load required packages
install.packages("randomForest", dependencies = TRUE)
install.packages("rpart", dependencies = TRUE)
install.packages("caret", dependencies = TRUE)
install.packages("e1071", dependencies = TRUE)
library(randomForest)
library(caTools)
library(caret)
library(rpart)
library(caret)
library(e1071)

#######################################################################################################
#######################################################################################################
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 using caTools
set.seed(111)
train <- sample.split(data3$Poker_Hands, SplitRatio = 0.70)
TrainSet<-subset(data3, train == TRUE)
dim(TrainSet)
ValidSet<-subset(data3, train == FALSE)
dim(ValidSet)

#######################################################################################################
#######################################################################################################
# Decision Tree approach
model_dt<-train(Poker_Hands ~ ., data = TrainSet, method = "rpart")
model_dt_1<-predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Poker_Hands)
mean(model_dt_1 == TrainSet$Poker_Hands)

# Running on Validation Set
model_dt_vs<-predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Poker_Hands)
mean(model_dt_vs == ValidSet$Poker_Hands)

#######################################################################################################
#######################################################################################################
# Create a Random Forest model with default parameters
model1 <- randomForest(Poker_Hands ~ ., data = TrainSet, importance = TRUE)
model1

# Fine tuning parameters of Random Forest model
model2 <- randomForest(Poker_Hands ~ ., data = TrainSet, ntree = 500, mtry = 5, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")

# Checking classification accuracy
a<-table(predTrain, TrainSet$Poker_Hands)
confusionMatrix(a)

# Predicting on Validation set
predValid <- predict(model1, ValidSet, type = "class")
a<-table(predValid, ValidSet$Poker_Hands)
confusionMatrix(a)
predValid <- predict(model2, ValidSet, type = "class")
b<-table(predValid, ValidSet$Poker_Hands)
confusionMatrix(b)

# Checking classification accuracy
mean(predValid == ValidSet$Poker_Hands)                
table(predValid,ValidSet$Poker_Hands)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 6:10) {
  model3 <- randomForest(Poker_Hands ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Poker_Hands)
}
a
plot(3:10,a)
#######################################################################################################
#######################################################################################################

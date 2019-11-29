setwd("C:/Users/arjun_0ozfyct/Documents/Clustring")

# Load the dataset and explore
data1<-data.frame(read.csv("poker-hand-training-true.csv", header = TRUE, stringsAsFactors = TRUE))

colnames(data1)<-c("Suit_of_card_1","Rank_of_card_1","Suit_of_card_2","Rank_of_card_2","Suit_of_card_3","Rank_of_card_3","Suit_of_card_4","Rank_of_card_4","Suit_of_card_5","Rank_of_card_5","Poker_Hands")

head(data1)

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
sum(is.na(data3$Poker_Hands))
library(caTools)
set.seed(111)
samp<-sample.split(data3$Poker_Hands, SplitRatio = 0.70)
TrainSet <- subset(data3, samp == TRUE)
ValidSet <- subset(data3, samp == FALSE)
str(TrainSet)
set.seed(1)
library(e1071)
svmfit=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=0.1,cost=1)
##Accuracy 71.73% ##
svmfit=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=0.1,cost=100)
##Accuracy 88.74% ##
summary(svmfit)
ypred=predict(svmfit,ValidSet)
ypred
a=table(predict=ypred,truth=ValidSet$Poker_Hands)
confusionMatrix(a)
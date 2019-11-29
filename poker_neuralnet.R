setwd("C:/Users/arjun_0ozfyct/Documents/Clustring")
data1<-data.frame(read.csv("poker-hand-training-true.csv", header = TRUE, stringsAsFactors = TRUE))


colnames(data1)<-c("Suit_of_card_1","Rank_of_card_1","Suit_of_card_2","Rank_of_card_2","Suit_of_card_3","Rank_of_card_3","Suit_of_card_4","Rank_of_card_4","Suit_of_card_5","Rank_of_card_5","Poker_Hands")

str(data1)
# max= apply(data1,2,max)
# min= apply(data1,2,min)
# scaled = as.data.frame(scale(data1, center = min, scale = max - min))
# View(scaled)
# scaled$Suit_of_card_1<-as.numeric(scaled$Suit_of_card_1)
# scaled$Suit_of_card_2<-as.numeric(scaled$Suit_of_card_2)
# scaled$Suit_of_card_3<-as.numeric(scaled$Suit_of_card_3)
# scaled$Suit_of_card_4<-as.numeric(scaled$Suit_of_card_4)
# scaled$Suit_of_card_5<-as.numeric(scaled$Suit_of_card_5)
# scaled$Rank_of_card_1<-as.numeric(scaled$Rank_of_card_1)
# scaled$Rank_of_card_2<-as.numeric(scaled$Rank_of_card_2)
# scaled$Rank_of_card_3<-as.numeric(scaled$Rank_of_card_3)
# scaled$Rank_of_card_4<-as.numeric(scaled$Rank_of_card_4)
# scaled$Rank_of_card_5<-as.numeric(scaled$Rank_of_card_5)
# str(scaled)
# library(caTools)
# set.seed(111)
# samp<-sample.split(scaled$Poker_Hands, SplitRatio = 0.70)
# train = subset(scaled, samp == TRUE)
# dim(train)
# test = subset(scaled, samp == FALSE)
# dim(test)
# str(train)
###Neural Net###
# library(neuralnet)
# feats<-names(train)
# feats<- feats[1:10]
# f<- paste(feats,collapse= '+')
# f
# f<-paste('Poker_Hands ~',f)
# f<- as.formula(f)
# f
# NN <- neuralnet(f,train, hidden = c(10,10,10))

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

library(caret)
library(nnet)
nnmodel<-train(TrainSet[,-11],TrainSet$Poker_Hands,method = "nnet", trControl = trainControl(method = 'cv',number = 10))
plot(nnmodel)

ypred=predict(nnmodel,ValidSet)
ypred
confusionMatrix(ypred,ValidSet$Poker_Hands)

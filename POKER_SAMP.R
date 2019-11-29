setwd("C:/Users/arjun_0ozfyct/Documents/Clustring")
data1<-data.frame(read.csv("poker-hand-training-true.csv", header = TRUE, stringsAsFactors = TRUE))

colnames(data1)<-c("Suit_of_card_1","Rank_of_card_1","Suit_of_card_2","Rank_of_card_2","Suit_of_card_3","Rank_of_card_3","Suit_of_card_4","Rank_of_card_4","Suit_of_card_5","Rank_of_card_5","Poker_Hands")
head(data1)
str(data1)
summary(data1)
table(data1$Poker_Hands)
# library(DMwR)
# nd <- SMOTE(data1$Poker_Hands ~ ., data1, perc.over = 600,perc.under=0)
#install.packages("ROSE")
# library(ROSE)
# if(data1$Poker_Hands == 1 & data1$Poker_Hands = 2)
# {
# data.rose <- ROSE(data1$Poker_Hands ~ ., data = data1, seed = 1)$data
# }else{
#   data.rose<-data1
# }
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
table(data3$Poker_Hands)
data1_2<-droplevels(subset(data3, ! Poker_Hands %in% c(0,3,4,5,6,7,8,9)))
table(data1_2$Poker_Hands)
data0<- droplevels(subset(data3, ! Poker_Hands %in% c(1,2,3,4,5,6,7,8,9)))
table(data0$Poker_Hands)
str(data1_2)
set.seed(123)
library(DMwR)
#library(ROSE)
library(caret)
#data.rose <- ROSE(Poker_Hands ~ ., data = data1_2)$data
#table(data.rose$Poker_Hands)
nd <- SMOTE(Poker_Hands ~ ., data = data1_2)
table(nd$Poker_Hands)
ndata0<- rbind(data0,nd)
table(ndata0$Poker_Hands)
str(ndata0)
ndata0<- droplevels(subset(ndata0, ! Poker_Hands %in% 2))
set.seed(123)
nd0 <-  downSample(x = ndata0[, -ncol(ndata0)],
                                 y = ndata0$Poker_Hands,yname = "Poker_Hands")
table(nd0$Poker_Hands)
nd2 <- droplevels(subset(nd, ! Poker_Hands %in% 1))
table(nd2$Poker_Hands)
###
dat3<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,4,5,6,7,8,9)))
dat3<- rbind(nd2,dat3)
table(dat3$Poker_Hands)
set.seed(123)
nd3 <- SMOTE(Poker_Hands ~ ., data = dat3)
table(nd3$Poker_Hands)
n3<- droplevels(subset(nd3, ! Poker_Hands %in% 2))
table(n3$Poker_Hands)
###
nd4<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,5,6,7,8,9)))
nd4<-rbind(n3,nd4)
table(nd4$Poker_Hands)
set.seed(123)
ndata4 <- upSample(x = nd4[, -ncol(nd4)],
              y = nd4$Poker_Hands,yname = "Poker_Hands")
table(ndata4$Poker_Hands)
n4<- droplevels(subset(ndata4, ! Poker_Hands %in% 3))
table(n4$Poker_Hands)
###
nd5<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,4,6,7,8,9)))
nd5<-rbind(n3,nd5)
table(nd5$Poker_Hands)
set.seed(123)
ndata5 <- upSample(x = nd5[, -ncol(nd5)],
                   y = nd5$Poker_Hands,yname = "Poker_Hands")
table(ndata5$Poker_Hands)
n5<- droplevels(subset(ndata5, ! Poker_Hands %in% 3))
table(n5$Poker_Hands)
###
nd6<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,4,5,7,8,9)))
nd6<-rbind(n3,nd6)
table(nd6$Poker_Hands)
set.seed(123)
ndata6 <- upSample(x = nd6[, -ncol(nd6)],
                   y = nd6$Poker_Hands,yname = "Poker_Hands")
table(ndata6$Poker_Hands)
n6<- droplevels(subset(ndata6, ! Poker_Hands %in% 3))
table(n6$Poker_Hands)
###
nd7<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,4,5,6,8,9)))
nd7<-rbind(n3,nd7)
table(nd7$Poker_Hands)
set.seed(123)
ndata7 <- upSample(x = nd7[, -ncol(nd7)],
                   y = nd7$Poker_Hands,yname = "Poker_Hands")
table(ndata7$Poker_Hands)
n7<- droplevels(subset(ndata7, ! Poker_Hands %in% 3))
table(n7$Poker_Hands)
###
nd8<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,4,5,6,7,9)))
nd8<-rbind(n3,nd8)
table(nd8$Poker_Hands)
set.seed(123)
ndata8 <- upSample(x = nd8[, -ncol(nd8)],
                   y = nd8$Poker_Hands,yname = "Poker_Hands")
table(ndata8$Poker_Hands)
n8<- droplevels(subset(ndata8, ! Poker_Hands %in% 3))
table(n8$Poker_Hands)
###
nd9<- droplevels(subset(data3, ! Poker_Hands %in% c(0,1,2,3,4,5,6,7,8)))
nd9<-rbind(n3,nd9)
table(nd9$Poker_Hands)
set.seed(123)
ndata9 <- upSample(x = nd9[, -ncol(nd9)],
                   y = nd9$Poker_Hands,yname = "Poker_Hands")
table(ndata9$Poker_Hands)
n9<- droplevels(subset(ndata9, ! Poker_Hands %in% 3))
table(n9$Poker_Hands)
#####
final_data<-rbind(nd0,nd2,n3,n4,n5,n6,n7,n8,n9)
table(final_data$Poker_Hands)
str(final_data)
# data5<-data3[! data3$Poker_Hands %in% c(1,2,3), ]
# table(data5$Poker_Hands)
#set.seed(9560)
#down_train <- downSample(x = data1_2[, -ncol(data1_2)],
           #              y = data1_2$Poker_Hands)

##########################################################################################
##################################SUPPORT VECTOR MACHINES################################
##########################################################################################
library(caTools)
set.seed(123)
samp<-sample.split(final_data$Poker_Hands, SplitRatio = 0.70)
TrainSet <- subset(final_data, samp == TRUE)
ValidSet <- subset(final_data, samp == FALSE)
str(TrainSet)
set.seed(123)
library(e1071)
svmfit=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=0.1,cost=1000)
##Accuracy 77.35%% ##
svmfit1=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=1,cost=1000)
##Accuracy 64.49%% ##
svmfit2=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=0.01,cost=1000)
##Accuracy 79.93%% ##
svmfit3=svm(Poker_Hands~.,data=TrainSet,kernel="radial",gamma=0.05,cost=1000)
##Accuracy 78.66%% ##
summary(svmfit3)
ypred=predict(svmfit3,ValidSet)
ypred
a=table(predict=ypred,truth=ValidSet$Poker_Hands)
confusionMatrix(a)
#table(down_train$Class) 
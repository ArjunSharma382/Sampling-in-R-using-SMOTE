# The intent of the project is to learn poker rules by applying different learning algorithms
# Approaches used:
# Decision tree
# Random forest
# Support Vector Machine
# Extreme Gradient Boosting
# Nueral Net

#######################################################################################################
####1st APPROACH DECISION TREE####
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
####2nd APPROACH RANDOM FOREST####
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
for (i in 3:10) {
  model3 <- randomForest(Poker_Hands ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Poker_Hands)
}
a
plot(3:10,a)
#######################################################################################################
####3rd Approach SVM###
#######################################################################################################
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
#######################################################################################################
####4th Approach XGBOOST####
#######################################################################################################
setwd("C:/Users/arjun_0ozfyct/Documents/Clustring")
library(xgboost)

data3<-data.frame(read.csv("poker-hand-training-true.csv", header = TRUE, stringsAsFactors = TRUE))
colnames(data3)<-c("Suit_of_card_1","Rank_of_card_1","Suit_of_card_2","Rank_of_card_2","Suit_of_card_3","Rank_of_card_3","Suit_of_card_4","Rank_of_card_4","Suit_of_card_5","Rank_of_card_5","Poker_Hands")
head(data3)
str(data3)
sum(is.na(data3$Poker_Hands))
library(caTools)
set.seed(111)
samp<-sample.split(data3$Poker_Hands, SplitRatio = 0.70)
TrainSet <- subset(data3, samp == TRUE)
ValidSet <- subset(data3, samp == FALSE)
str(TrainSet)
train_label <- TrainSet$Poker_Hands
train_matrix <- xgb.DMatrix(data = as.matrix(TrainSet[,-11]), label = train_label)
test_label <- ValidSet$Poker_Hands
test_matrix <- xgb.DMatrix(data = as.matrix(ValidSet[,-11]), label = test_label)
watchlist <- list(train = train_matrix, test = test_matrix)
xgb_params <- list("objective" = "multi:softmax",
                   "eval_metric" = "mlogloss")

# eXtreme Gradient Boosting Model
bst_model2 <- xgb.train(params = xgb_params,
                        data = train_matrix,
                        watchlist = watchlist,
                        eta = 0.1,
                        gamma = 0.3,
                        max_depth = 5, 
                        nround=500, 
                        subsample = 0.7,
                        colsample_bytree = 0.7,
                        seed = 333,
                        num_class=10,
                        verbose=1)

ypred=predict(bst_model2,test_matrix)
ypred
label = getinfo(test_matrix, "label")
err <- as.numeric(sum(as.integer(ypred > 0.5) != label))/length(label)
print(paste("test-error=", err))

e <- data.frame(bst_model2$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
y_pred2 <- predict(bst_model2, data.matrix(ValidSet[,-11]))
y_pred2<-as.data.frame(y_pred2)  
results2<-cbind(ValidSet$Poker_Hands,y_pred2)
table(results2$y_pred2,ValidSet$Poker_Hands)
acc<-100*sum(results2$y_pred2 == results2$`ValidSet$Poker_Hands`)/nrow(results2)
print(paste("Accuracy=", acc))
# 71.3847% accuracy
#important variables...
importance_matrix2 <- xgb.importance(model = bst_model2)
print(importance_matrix2)
xgb.plot.importance(importance_matrix = importance_matrix2)
#######################################################################################################
####5th Approach NEURAL-NET####
#######################################################################################################
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
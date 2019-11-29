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
                        eta = 0.01,
                        gamma = 0.3,
                        max_depth = 8, 
                        nround=1000, 
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
######
#####
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

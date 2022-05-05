#Random Forest
install.packages("randomForest")
library(randomForest)

#Get data
#universal bank
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[,-c(1,5)]

#Data Partitioning
set.seed(1)
train.index <- sample(c(1:dim(bank.df)[1]),
                      dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

rf <- randomForest(as.factor(Personal.Loan) ~ .,
                   data = train.df,
                   ntree = 500,
                   mtry = 4, nodesize = 5, importance = TRUE)

par(mar=c(1,1,1,1))
varImpPlot(rf, type=1)

#install.packages("caret")
library(caret)

rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, factor(valid.df$Personal.Loan))

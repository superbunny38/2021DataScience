#Random Forest
library(randomForest)
rf <- randomForest(as.factor(Personal.Loan) ~ .,
                   data = train.df,
                   ntree = 500,
                   mtry = 4,
                   nodesize = 5,
                   importance = TRUE)

rf.pred <- predict(rf, train.df)#build tree to explain randomforest

#Boost Tree
library(adabag)
train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data = train.df)
boost.pred <- predict(boost, train.df)

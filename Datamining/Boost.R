#Boost Tree
library(adabag)

train.df$Personal.Loan <- as.factor(train.df$Personal.Loan)
boost <- boosting(Personal.Loan ~ ., data = train.df)

valid.df$Personal.Loan <- as.factor(valid.df$Personal.Loan)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), valid.df$Personal.Loan)


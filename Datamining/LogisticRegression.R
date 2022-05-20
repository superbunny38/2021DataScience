library(caret)
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[-c(1,5)]
bank.df$Education <- factor(bank.df$Education, levels = c(1,2,3),
                            labels = c("Undergrad","Graduate","Advanced/Professional"))
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)

train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]
#train
logit.reg <- glm(Personal.Loan ~ ., data = train.df,
                 family = "binomial")
options(scipen = 999)
summary(logit.reg)

#validation
valid.df$Personal.Loan <- as.factor(valid.df$Personal.Loan)

pred.valid <- predict(logit.reg, valid.df, type = "response")#확률로 나옴 
data.frame(actual = valid.df$Personal.Loan[1:20],
           predicted = pred.valid[1:20])

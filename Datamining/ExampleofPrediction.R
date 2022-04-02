# (1) Encode Categorical Variables
data = read.csv("ToyotaCorolla.csv")
dummy <- dummyVars(" ~ .", data=data)
final_df <- data.frame(predict(dummy, newdata=data))

#training set
training.rows <- sample(row.names(final_df), dim(final_df)[1]*0.5)
train.df <- final_df[training.rows,]

#validation set
validation.rows <- sample(setdiff(row.names(final_df),training.rows),dim(final_df)[1]*0.3)
valid.df <- final_df[validation.rows,]

#test set
test.rows <- setdiff(row.names(final_df), union(training.rows, validation.rows))
test.df <- final_df[test.rows,]


#학습 데이터를 이용한 학습 및 학습 성능 파악
#Predict Price

colnames(final_df)
sum(is.na(final_df))

#train model
reg <- lm(Price ~ ., data = final_df, subset =training.rows)
tr.res <- data.frame(train.df$Price, reg$fitted.values, reg$residuals)
head(tr.res)

#validation
pred = predict(reg, newdata = valid.df)
vl.res <- data.frame(valid.df$Price, pred, residuals=valid.df$Price - pred)
head(vl.res)

#test
test = predict(reg, newdata = test.df)
te.res <- data.frame(test.df$Price, test, residuals = test.df$Price-test)
head(te.res)

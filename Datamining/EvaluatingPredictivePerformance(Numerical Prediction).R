#Prediction error metrics from a  model Toyota Car prices.
#Training and Validation

library(forecast)#required to evaluate performance

#load file
toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
head(toyota.corolla.df)
sum(is.na(toyota.corolla.df))

#randomly generate training and validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(setdiff(toyota.corolla.df$Id, training),400)
sum(is.na(toyota.corolla.df[validation,]))
sum(is.na(toyota.corolla.df[training,]))

#run linear regression model
head(toyota.corolla.df[,c(1,2,8,11)])#쓰지 않을 columns
reg <- lm(Price~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training,
          na.action = na.exclude)#결측치가 있는 부분의 잔차를 결측치로 삽입하여 데이터의 길이를 그대로 유지

#training accuracy
pred_t <- predict(reg, na.action = na.pass)#데이터프레임에 결측치가 있어도 그대로 사용

#validation accuracy
pred_v <- predict(reg, newdata = toyota.corolla.df[validation, -c(1,2,8,11)],
                  na.action = na.pass)

##evaluate performancde
#training
tr.acc <- accuracy(pred_t, toyota.corolla.df[training,]$Price)
tr.acc <- as.data.frame(tr.acc)

#validation
val.acc <- accuracy(pred_v, toyota.corolla.df[validation,]$Price)
val.acc <- as.data.frame(val.acc)

tr.acc$type <- "Train"
val.acc$type <- "Validation"

acc <- rbind(tr.acc, val.acc)
acc <- as.data.frame(acc)
acc


# A lift chart and decile-wise lift chart

toyota.corolla.df <- read.csv("ToyotaCorolla.csv")
sum(is.na(toyota.corolla.df[validation,]$Price))#NA 개수 3개

#remove missing Price data
dim(toyota.corolla.df)#1436 39
toyota.corolla.df <- toyota.corolla.df[!is.na(toyota.corolla.df[validation,]$Price),]#Validatation dataset에 있는 NA 삭제
dim(toyota.corolla.df)#1427 39

#generate random Training and validation sets
training <- sample(toyota.corolla.df$Id, 600)
validation <- sample(toyota.corolla.df$Id, 400)

#regression model based on all numerical predictors
reg <- lm(Price ~., data = toyota.corolla.df[,-c(1,2,8,11)], subset = training)

#predictions
pred_v <- predict(reg, newdata = toyota.corolla.df[validation,-c(1,2,8,11)])

#load package gains
#install.packages("gains")
library(gains)
gain <- gains(toyota.corolla.df[validation,]$Price[!is.na(pred_v)], pred_v[!is.na(pred_v)])
head(gain)

#cumulative lift chart
options(scipen = 999)#avoid scientific notation
#compute relative gain to price
price <- toyota.corolla.df[validation,]$Price[!is.na(toyota.corolla.df[validation,]$Price)]


plot(c(0,gain$cume.pct.of.total*sum(price))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative Price", main="Lift Chart", type="l")

#baseline
lines(c(0,sum(price))~c(0,dim(toyota.corolla.df[validation,])[1]), col = "grey", lty = 2)

#Decile-wise(10분위 수) lift chart
barplot(gain$mean.resp/mean(price), names.arg = gain$depth,
        xlab = "Percentile", ylab = "Mean Response",main = "Decile-wise lift chart")

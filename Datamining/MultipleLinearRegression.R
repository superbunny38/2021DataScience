### Linar regression model of price vs. car attributes
car.df <- read.csv("ToyotaCorolla.csv")

#use first 1000 rows of data
car.df <- car.df[1:1000,]
dim(car.df)
head(car.df)

#select variables for regression
selected.var <- c(3,4,7,8,9,10,12,13,14,17,18)

#partition data
set.seed(1)#set seed for reproducing the partition

train.index <- sample(c(1:1000),600)
train.df <- car.df[train.index,selected.var]
valid.df <- car.df[-train.index, selected.var]

#Use lm() to run a linear regression of Price on all 11 predictors in the training set.
#use . after ~ to include all the remaining columns in train.df as predictors.

car.lm <- lm(Price ~., data=train.df)

#Use options() to ensure the numbers are not displayed in scientific notation.
options(scipen = 999)
summary(car.lm)

### Predicted Prices (and Errors) for 20 cars in validation set and summary predictive measures for entire validation set

library(forecast)

car.lm.predict <- predict(car.lm, valid.df)#make predictions on a new set
options(scipen = 999, digits = 0)

some.residuals <- valid.df$Price[1:20] - car.lm.predict[1:20]#y-y_hat
data.frame("Predicted" = car.lm.predict[1:20], "Actual" = valid.df$Price[1:20], "Residual" = some.residuals)

accuracy(car.lm.predict, valid.df$Price)#compute common accuracy measures

#code for plotting histogram of validation errors
library(forecast)
car.lm.predict <- predict(car.lm, valid.df)
all.residuals <- valid.df$Price - car.lm.predict
class(all.residuals)
hist(all.residuals, breaks = 25, xlab="Residuals", main="")
length(all.residuals[which(all.residuals<-1460 & all.residuals <1460)])/400#여기에 넣어야 실행됨,, 이 코드가 뭐하는 거지?

hist(valid.df$Price)



## Exhaustive search for reducing predictors in toyota corolla example

#unlike with lm, categorical predictors must be turned into dummies manually.


library(leaps)#leaps 패키지 불러오기
class(car.df$Fuel_Type)
head(train.df$Fuel_Type)
Fuel_Type <- as.data.frame(model.matrix(~0+Fuel_Type,data=train.df))#One-hot encoding
head(Fuel_Type)
#train.df[,-4]#Fuel_Type column 제외

train.df <- cbind(train.df[,-4],Fuel_Type[,c(1,2)])#Fuel_Type column 제외하고 encoding한 data 합치기
head(train.df)

search <- regsubsets(Price ~., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)

#show models
sum$which

#show metrics
sum$rsq#결과가 이상한데..?
#R squared values range from 0 to 1.. but.. all 1s
sum$adjr2
sum$cp


## Backward Elimination for reducing predictors in toyota corolla example

#use step() to run stepwise regression
#set directions = to either "backward", "forward", or "both".

car.lm <- lm(Price ~., data = train.df)
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step)#which variables did it drop
head(car.lm.step)

Fuel_Type_val <- as.data.frame(model.matrix(~0 + Fuel_Type, data = valid.df))
valid.df <- cbind(valid.df[,-4], Fuel_Type_val[,c(1,2)])
car.lm.step.pred <- predict(car.lm.step, valid.df)
accuracy(car.lm.step.pred, valid.df$Price)


## Forward Elimination for reducingpredictors in toyota corolla example
car.lm.forward <- step(car.lm, direction = "forward")
summary(car.lm.forward)

#Stepwise Regression
car.lm.stepwise <- step(car.lm, direction = "both")
summary(car.lm.stepwise)

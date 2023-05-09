#install.packages("nnet")
library(nnet)

d.olive <- read.csv("olive.csv",header = TRUE,sep = ",")
d.olive$Region <- as.factor(d.olive$Region)# int to factor for the class variable
table(d.olive$Region)

#error에 미치는 영향을 비슷하게 하기 위해서
d.olive[,4:11]<-scale(d.olive[,4:11],center = TRUE,scale = TRUE)#standardize x variables for neural network

#stratified random partitioning
d.olive <- d.olive[,-c(1,3)]
table(d.olive$Region)
d.olive.1 <- d.olive[d.olive$Region == 1,]
d.olive.2 <- d.olive[d.olive$Region == 2,]
d.olive.3 <- d.olive[d.olive$Region == 3,]

dim(d.olive.1)
dim(d.olive.2)
dim(d.olive.3)

#shuffling
nr.1 <- nrow(d.olive.1)
nr.2 <- nrow(d.olive.2)
nr.3 <- nrow(d.olive.3)
d.olive.1 <- d.olive.1[sample(nr.1),]
d.olive.2 <- d.olive.2[sample(nr.2),]
d.olive.3 <- d.olive.3[sample(nr.3),]

#stratified partition
d.olive.1.train <- d.olive.1[1:round(nr.1*0.7),]
d.olive.1.test <- d.olive.1[(round(nr.1*0.7)+1):nr.1,]
dim(d.olive.1.train)
dim(d.olive.1.test)
head(d.olive.1.train)

d.olive.2.train <- d.olive.2[1:round(nr.2*0.7),]
d.olive.2.test <- d.olive.2[(round(nr.2*0.7)+1):nr.2,]
dim(d.olive.2.train)
dim(d.olive.2.test)

d.olive.3.train <- d.olive.3[1:round(nr.3*0.7),]
d.olive.3.test <- d.olive.3[(round(nr.3*0.7)+1):nr.3,]
dim(d.olive.3.train)
dim(d.olive.3.test)

d.olive.train <- rbind(d.olive.1.train, d.olive.2.train, d.olive.3.train)
d.olive.test <- rbind(d.olive.1.test, d.olive.2.test, d.olive.3.test)
dim(d.olive.train)
dim(d.olive.test)

table(d.olive.train$Region)

#encoding class variable
nny.train <- class.ind(d.olive.train[,1])#function in nnet package
nny.train#one hot encoded target
d.olive.train <- cbind(nny.train,d.olive.train[,-1])
head(d.olive.train)#target 값이 one-hot-encoding된 train data

table(d.olive.train$'1')
table(d.olive.train$'2')
table(d.olive.train$'3')

nny.test <- class.ind(d.olive.test[,1])
head(nny.test)#one hot encoded target
d.olive.test <- cbind(nny.test,d.olive.test[,-1])
head(d.olive.test)
table(d.olive.test$'1')
table(d.olive.test$'2')
table(d.olive.test$'3')

# training a neural network (input vars, target vars, hidden node 개수, max iterations)
nn.olive <- nnet(d.olive.train[,4:11],d.olive.train[,1:3],size = 3, maxit = 100)#nnet library에선 error function을 batch learning으로 함
nn.olive

nn.olive$fitted.values#예측 값들

# training error
pred.y <- apply(nn.olive$fitted.value,1,which.max)
pred.y#predicted values from train

true.y <- apply(d.olive.train[,1:3],1,which.max)
table(pred.y,true.y)#다 맞춤
train.err <- sum(pred.y != true.y)/length(pred.y)

dim(d.olive.train[,4:11])
dim(d.olive.test[,4:11])

#test error
preds.one.hot <-predict(nn.olive, d.olive.test[,4:11], type = "raw")
pred.y <- apply(preds.one.hot, 1, which.max)
true.y <- apply(d.olive.test[,1:3],1,which.max)
test.err <- sum(pred.y != true.y)/length(pred.y)
table(pred.y,true.y)
test.err

nn.olive$wts#학습된 final weights값들을 return 해줌

summary(nn.olive)#weight 위치와 값을 알려줌

#observing  error (objective function value)
#get initial weights
init.wts <- rep(0,39)

max.epoch <- 100
err.vec <- rep(0,max.epoch)
err.vec

for(i in 1:max.epoch){
  nn.olive <- nnet(d.olive.train[,4:11],d.olive.train[,1:3],Wts = init.wts, size = 3, maxit = i)#max iteration을 i만큼
  em <- nn.olive$fitted.value - d.olive.train[,1:3]
  err.vec[i] <- sum(em*em)#element wise multiplication (squared error 계산)
}

#plot errors
plot(err.vec, type = "l", xlab = "epoch", ylab = "error")

####### Regression

#generating data where nonlinear relationship exists
x<- seq(-6,6,0.5)
y <- 1/(1+exp(-x))+rnorm(length(x),0,0.03)
plot(x,y)
nn.reg <- nnet(x,y,size = 3, maxit = 100)

#drawing fitted curve
new.x <- data.frame(x = seq(-6,6,0.01))
new.y <- predict(nn.reg,new.x, type = "raw")
plot(x,y)
lines(new.x[,1],new.y)

summary(nn.reg)

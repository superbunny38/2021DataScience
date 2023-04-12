library(class)
help('knn')

#knn(train, test, cl, k = 3, prob=TRUE)
#train: train set
#test: test set (예측하고 싶은 데이터)
#cl: factor type의 train set에 대한 class 값들
#k

d.music <- read.csv("music.csv",header = TRUE, sep = ",")
d.music <- d.music[,-c(1,2)]
table(d.music$Type)

train.idx <- sample(1:54,30)#54개 중에 30개
test.idx <- setdiff(1:54,train.idx)

d.music.train <- d.music[train.idx,]
d.music.test <- d.music[test.idx,]

pred.test <-knn(d.music.train[,-1],d.music.test[,-1],d.music.train$Type,k = 5)
table(d.music.test$Type, pred.test)

#label distribution 확인
table(d.music.train$Type)
table(d.music.test$Type)

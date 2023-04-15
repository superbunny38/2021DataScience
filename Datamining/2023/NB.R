library(e1071)
#install.packages("mlbench")
library(mlbench)

data("HouseVotes84")
help("HouseVotes84")#데이터에 대한 설명 
head(HouseVotes84)#missing value (NA) 존재

model <- naiveBayes(Class ~ ., data = HouseVotes84)
model#모든 conditional probability 들어있음

pred <- predict(model, HouseVotes84[,-1], method = 'class')#예측 방법
table(HouseVotes84[,1],pred)#train set에 대한 confusion matrix

# Laplace도 해보시길 바람
# Train Test Set도 나눠서 acc 찍어보길 바람

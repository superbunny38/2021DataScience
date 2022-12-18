############ Multiple Linear Regression for Boston Housing Dataset

#read and check data
boston <- read.csv("boston.csv")#boston.csv 파일 읽어오기
boston <- boston[1:500,]#편의상 500 rows만 선택
head(boston)#첫 여섯행 출력

summary(boston)
str(boston)
dim(boston)
names(boston)

set.seed(1)#seed값 고정

#check correlations
cor(boston$CRIM, boston$MEDV)#-0.388
cor(boston$ZN, boston$MEDV)#0.36
cor(boston$INDUS, boston$MEDV)#-0.484
cor(boston$CHAS, boston$MEDV)#0.175 <- 4번째 변수 제외 (상관관계가 너무 낮음)
cor(boston$NOX, boston$MEDV)#-0.427
cor(boston$RM, boston$MEDV)#0.695
cor(boston$AGE, boston$MEDV)#-0.377
cor(boston$DIS, boston$MEDV)#0.25
cor(boston$RAD, boston$MEDV)#-0.382
cor(boston$TAX, boston$MEDV)#-0.469 <- 10번째 변수 제외 (집을 사고 난 후 세금이 책정됨)
cor(boston$PTRATIO, boston$MEDV)#-0.508
cor(boston$B, boston$MEDV)#0.333
cor(boston$LSTAT, boston$MEDV)#-0.738
cor(boston$CAT..MEDV, boston$MEDV)#0.79 <- 15번째 변수 제외 (CAT..MEDV은 $30,000을 넘은지에 대한 변수이기에 집값 책정 후 결정됨)


#select variables
selected.var2 <- c(1:3,5:9,11:14)#필요한 변수 번호만 저장

n.train <- dim(boston)[1]*0.6#60% for training dataset

train.index <- sample(c(1:500),n.train)#training data rows 선택
train.df <- boston[train.index, selected.var2]#training dataset
valid.df <- boston[-train.index, selected.var2]#validation datset (training dataset에 할당한 row는 제외)

house.lm <- lm(MEDV ~., data = train.df)#linear model 형태로 training dataset으로 학습 해 MEDV를 예측하는 모델 생성
options(scipen = 999)#scipen: 지수표기를 숫자표기로 바꿈
summary(house.lm)#모델 요약

house.lm.pred <- predict(house.lm, valid.df)#car.lm 모델을 이용해서 validation data를 예측
options(scipen = 999, digits = 0)#지수표기를 숫자표기로 바꿈 + 소수점 출력 안 함
some.residuals <- valid.df$MEDV[1:20]-house.lm.pred[1:20]#잔차 계산 (y-y_hat)

data.frame("Predicted" = house.lm.pred[1:20],#예측한 값
           "Actual" = valid.df$MEDV[1:20],#실제 값
           "Residual" =some.residuals)#잔차

library(forecast)#forecast 패키지 불러오기

options(scipen = 999, digits = 3)#지수표기를 숫자표기로 바꿈 + 소수점 두 번째 자리까지 표시
accuracy(house.lm.pred, valid.df$MEDV)
all.residuals <- valid.df$MEDV - house.lm.pred

#length(all.residuals[which(all.residuals >- 1406&all.residuals<1406)])/400

par(mar=c(1,1,1,1))#시각화 시 Error 해결하기 위한 코드
hist(all.residuals, breaks = 30, xlab = "Residuals", main = "")#히스토그램을 통한 잔차 시각화

boxplot(all.residuals)#잔차를 박스플롯으로 시각화

############ Selecting Subsets
library(leaps)
str(boston)#nothing of a character value (No need for encoding categorical values)

#Exhaustive Search
head(train.df)

#Exhaustive Search
search <- regsubsets(MEDV ~., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")
sum <- summary(search)
sum$which

sum$rsq#R-square

sum$adjr2#Adjusted R-square

sum$cp#Mallow's Cp

#Forward Selection
house.lm <- lm(MEDV ~ ., data = train.df)
colnames(train.df)
length(colnames(train.df))

house.lm.step <- step(house.lm, direction = "forward")#Forward Selection
summary(house.lm.step)#Forward Selection 요약: 모든 변수 다 쓰임

#Backward Selection

house.lm <- lm(MEDV ~., data = train.df)#train linear model
house.lm.step <- step(house.lm, direction = "backward")#backward selection
summary(house.lm.step)#summary of backward selection

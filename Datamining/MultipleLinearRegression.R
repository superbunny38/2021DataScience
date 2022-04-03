#R-Exercise: MLR (1)

car.df <- read.csv("ToyotaCorolla.csv")#csv 파일 불러오기
car.df <- car.df[1:1000,]#1000 rows 선택
head(car.df)#첫 여섯 행 출력
dim(car.df)#data size
summary(car.df)#요약
str(car.df)#구조
names(car.df)#feature names

selected.var <- c(3,4,7:10,12:14,17,18)#변수 index 선택
length(selected.var)#11개 변수 선택
selected.var#선택된 변수 번호

set.seed(1)#항상 동일한 값이 나오도록 seed값 고정

train.index <- sample(c(1:1000),600)#training data rows 선택
train.df <- car.df[train.index, selected.var]#training dataset
valid.df <- car.df[-train.index, selected.var]#validation datset (training dataset에 할당한 row는 제외)

car.lm <- lm(Price ~., data = train.df)#linear model 형태로 training dataset으로 학습 해 Price를 예측하는 모델 생성
options(scipen = 999)#scipen: 지수표기를 숫자표기로 바꿈
summary(car.lm)#모델 요약


#R-Exercise: MLR (2)

car.lm.pred <- predict(car.lm, valid.df)#car.lm 모델을 이용해서 validation data를 예측

options(scipen = 999, digits = 0)#지수표기를 숫자표기로 바꾸고 소수점 출력 안 함
some.residuals <- valid.df$Price[1:20]-car.lm.pred[1:20]#잔차 계산 (y-y_hat)
data.frame("Predicted" = car.lm.pred[1:20],#예측한 값
           "Actual" = valid.df$Price[1:20],#실제 값
           "Residual" =some.residuals)#잔차

library(forecast)#forecast 패키지 불러오기
options(scipen = 999, digits = 3)#지수표기를 숫자표기로 바꿈 + 소수점 두 번째 자리까지 표시
accuracy(car.lm.pred, valid.df$Price)#정확성 관련 오류


#R-Exercise: MLR (3)
all.residuals <- valid.df$Price - car.lm.pred
length(all.residuals[which(all.residuals >- 1406&all.residuals<1406)])/400

#잔차를 25개 범위로 나눠 히스토그램을 통한 시각화
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")

boxplot(all.residuals)#잔차를 박스 플롯으로 시각화


#R-Exercise: Selecting Subsets (1)
#install.packages("leaps")

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
sum$which


#R-Exercise: Selecting Subsets (2)

sum$rsq

sum$adjr2

sum$cp


#R-Exercise: Selecting Subsets (3)

car.lm <- lm(Price ~ ., data = train.df)#train linear model
car.lm.step <- step(car.lm, direction = "forward")#forward selection
summary(car.lm.step)#전진 선택 결과 요약(모든 변수 다 쓰임)


#R-Exercise: Selecting Subsets (4)

car.lm <- lm(Price ~., data = train.df)#train a linear model
car.lm.step <- step(car.lm, direction = "backward")#backward selection
summary(car.lm.step)#후진 선택 요약


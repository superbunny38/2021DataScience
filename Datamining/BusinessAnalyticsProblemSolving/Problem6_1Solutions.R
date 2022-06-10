################## 
#      6.1       #
#   2018312824   #
#     류채은     #
##################

###### a
library(tidyverse)
b.df <- read_csv("BostonHousing.csv")
b.df <- na.omit(b.df)

#교재와 데이터를 동일하게 만들기 위해 변수 "CAT. MEDV" 제거
b.df <- b.df[,-14]

#Partition dataset
set.seed(5)
train.rows <- sample(c(1:dim(b.df)[1]),300)#300 indices for samples for training
train.df <- b.df[train.rows,]#train dataset
valid.df <- b.df[-train.rows,]#validation dataset

###### b
options(digits = 2)
reg <- lm(MEDV ~ CRIM+CHAS+RM, data = train.df)#train multiple linear regressor
summary(reg)#summary of multiple linear regression model

###### c
library(dplyr)
to.predict <- data.frame("CHAS"=0.0,"CRIM"= 0.1,"RM"=6.0)#a sample to predict
pred <- predict(reg, newdata = to.predict)
message(pred)#print predicted value

actual <-valid.df %>%
  filter(CHAS == 0, CRIM == 0.1, floor(RM) == 6)#get the actual value for the features for validation data
actual$MEDV
prediction.error <- actual$MEDV - pred#caculate prediction error
message(prediction.error)

###### d 

#i. 다중 공선성(Multi Collinearity) 확인
X = data.frame(scale(train.df$INDUS),scale(train.df$NOX),scale(train.df$TAX))#scale data and make a dataframe
colnames(X) <- c("INDUS","NOX","TAX")#set column names
X = as.matrix(X)
cor(X)#calculate correlation between variables

#Calculating Variance Infliation Factor(VIF) to calculate precise multi collinearity
#install.packages("car")
library(car)
car::vif(lm(train.df$MEDV ~ train.df$INDUS + train.df$NOX + train.df$TAX))

#Visualization of scatter matrices to visually infer muli collinearity
pairs(X, panel = panel.smooth)

#ii. 
X = data.frame(scale(train.df$CRIM),scale(train.df$ZN),scale(train.df$INDUS),scale(train.df$CHAS),scale(train.df$NOX),scale(train.df$RM),
               scale(train.df$AGE),scale(train.df$DIS),scale(train.df$RAD),scale(train.df$TAX),scale(train.df$PTRATIO),
               scale(train.df$LSTAT))
colnames(X) <- colnames(train.df)[1:12]
X = as.matrix(X)
cor(X)
library(corrplot)
corrplot(cor(X),
         method = "color",#색깔로 표현
         type = "lower",#왼쪽 아래 행렬만 표시,
         order = "hclust",
         addCoef.col = "black",#상관계수 색깔: 검정
         tl.col = "black",
         tl.srt = 0,#위 변수 명 눞이기기
         diag = F)#대각 성분 제외

#12개의 수치형 변수들 한해 VIF 값 계산
reg2 <- lm(MEDV ~ ., data = train.df)
vif(reg2)

#iii.
library(leaps)#leaps 패키지 불러오기
search <- regsubsets(MEDV ~., data = train.df, nbest = 1, nvmax = dim(train.df)[2], method = "exhaustive")

#Bayesian Information Criterion(BIC) 기준 최상위 모델 3개 시각화
plot(search, scale="bic")#BIC 기준으로 선택

#첫 번째로 좋은 모델
first.reg <- lm(MEDV ~ LSTAT, data = train.df)

#두 번째로 좋은 모델
second.reg <- lm(MEDV ~ LSTAT+RM, data = train.df)

#세 번째로 좋은 모델
third.reg <- lm(MEDV ~ LSTAT+RM+PTRATIO, data = train.df)

first.pred <- predict(first.reg, valid.df)#predict validation data with the best model
second.pred <- predict(second.reg, valid.df)#predict validation data with the second best model
third.pred <- predict(third.reg, valid.df)#predict validation data with the third best model

first.acc <- accuracy(first.pred, valid.df$MEDV)#accuracy of the best model's prediction
second.acc <- accuracy(second.pred, valid.df$MEDV)#accuracy of second best model's prediction
third.acc <- accuracy(third.pred, valid.df$MEDV)#accuracy of the third best model's prediction

#transpose the accuracy vectors for three models
first.acc.t <- t(first.acc)
second.acc.t <- t(second.acc)
third.acc.t <- t(third.acc)

df <- data.frame(first.acc.t,second.acc.t, third.acc.t)#combine all into one dataframe
rownames(df) <- rownames(first.acc.t)#change row names
colnames(df) <- c("first","second","third") #change column names of dataframe

df

#lift chart
#install.packages("gains")
library(gains)
actual = valid.df$MEDV
gain1 = gains(actual, first.pred, group = 10)
gain2 = gains(actual, second.pred, group = 10)
gain3 = gains(actual, third.pred, group = 10)
plot(c(0, gain1$cume.pct.of.total*sum(actual))~c(0, gain1$cume.obs), type = "l", xlab = "#Cases", ylab = "Cumulative MEDV", main = "Lift Chart for exhaustive search", col="blue",lwd = 2)
lines(c(0, gain2$cume.pct.of.total*sum(actual))~c(0, gain2$cume.obs),col="brown",lwd = 3)
lines(c(0, gain3$cume.pct.of.total*sum(actual))~c(0, gain3$cume.obs),col="grey",lwd=3)
#dotted-line for half segmentation
segments(0, 0, nrow(valid.df), sum(actual), lty = "dashed", col = "red", lwd = 3)
#add legend
legend(x = "bottomright",          # Position
       legend = c("first", "second","third"),  # Legend texts
       col = c("blue", "brown","pink","red"),           # Line colors
       lwd = 3) 


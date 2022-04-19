library(e1071)
delays.df <- read.csv("FlightDelays.csv")
delays.df.org <- delays.df
head(delays.df)

#change numerical variables -> categorical variables
delays.df$DAY_WEEK <- factor(delays.df.org$DAY_WEEK)
delays.df$DEP_TIME <- factor(delays.df.org$DEP_TIME)

delays.df$CRS_DEP_TIME <- factor(round(delays.df$CRS_DEP_TIME/100))

selected.var <- c(10,1,8,4,2,13)
names(delays.df)
names(delays.df)[selected.var]#variables to use

train.index <- sample(c(1:dim(delays.df)[1]), dim(delays.df)[1]*0.6)
train.df <- delays.df[train.index, selected.var]
valid.df <- delays.df[-train.index, selected.var]

delays.nb <- naiveBayes(Flight.Status ~ ., data =train.df)#Target ~ 나머지 4개 변수
delays.nb


prop.table(table(train.df$Flight.Status, train.df$DEST),margin = 1)

#항공기 지연 나이브 베이즈 분류를 이용한 예측
pred.prob <- predict(delays.nb, newdata = valid.df, type = "raw")#prob
pred.class <- predict(delays.nb, newdata = valid.df)#실제 class를 predict

df <- data.frame(actual = valid.df$Flight.Status, predicted = pred.class, pred.prob)
head(df)

df[valid.df$CARRIER == "DL" & valid.df$DAY_WEEK == 7 & valid.df$CRS_DEP_TIME == 10 & valid.df$DEST == "LGA" & valid.df$ORIGIN == "DCA",]

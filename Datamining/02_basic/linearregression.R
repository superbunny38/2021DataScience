#File/New File/R Script

#R에서 .은 의미가 없음

#####데이터 셋 불러오기와 내용 보기
#실행: Ctrl+Enter

#데이터 셋 불러오기 및 기본 정보 보기
housing.df <- read.csv("WestRoxbury.csv", header=TRUE)
dim(housing.df)#rowxcolumn
head(housing.df)#show first 6 rows
View(housing.df)#뷰어창에서 데이터 확인


#데이터 내용 보기
#[row, column]
housing.df[1:10,1]#R에서 "TOTAL VALUE"를 TOTAL.VAUE로 처리
housing.df[1:10,]#10 rows, 모든 columns
housing.df[0,]#column feature names
housing.df[5,1:10]
colnames(housing.df)#get column names
housing.df[6,c(1:2,4,8:10)]#c: vector, 8:10 = 8,9,10
housing.df[,1]#***첫번째*** column의 모든 값들
housing.df$TOTAL.VALUE#column name에 .넣어도 됨
housing.df$TOTAL.VALUE[1:10]#1,2,3,4,...,10
length(housing.df$TOTAL.VALUE)#len
length(housing.df$TOTAL.VALUE) == dim(housing.df)[1]#TRUE
mean(housing.df$TOTAL.VALUE)
summary(housing.df)



#####샘플링 및 오버샘플링


#데이터 샘플링
#sample(출처, 몇 개)
s1<-sample(row.names(housing.df),5)#housing.df의 열 이름 중 5개 가져오기
print(s1)#가져온 데이터 확인(index 번호 5개)

#오버샘플링
#90배 오버 샘플링(0.01*90=0.9)
s2<-sample(row.names(housing.df),5,prob = ifelse(housing.df$ROOMS>10,0.9,0.01))#샘플링 확률을 $ROOMS의 숫자에 따라 다르게 함, 10개 보다 큰 경우(if): 0.9, 아닌 경우(else): 0.01

print(s2)

#sampled된 rooms확인
housing.df[s,]$ROOMS

#housing.df[s]$ROOMS: 이렇게 하면 오류남


#####변수 설명 및 가변수 생성

##변수 설명

#변수명: names()
names(housing.df)#변수 명
t(names(housing.df))#transpose
t(t(names(housing.df)))
t(t(names(housing.df))) == names(housing.df)#TRUE이긴 한데 약간 다름

#column 이름: colnames()
colnames(housing.df)[1]#첫번째 column name
colnames(housing.df)[1] <- c("TOTAL_VALUE")#첫번째 column 이름 바꾸기

#변수 종류: class()
class(housing.df$REMODEL)#REMODEL 변수의 종류:character
class(housing.df$TOTAL_VALUE)#numeric

##가변수 생성
#데이터프레임 내의 변수를 (수치로 구성된) 메트릭스 형태로   변환

#~0: model without intercept
#REMODEL(categorical variable)을 subcategory로 나누고 가변수를 만듦
xtotal <- model.matrix(~0+BEDROOMS+REMODEL,data = housing.df)#matrix로 변환
xtotal <- as.data.frame(xtotal)#매트릭스 가시화를 위해 데이터 프레임으로 변환
#head(xtotal)



##### 결측치 대체

#테스트를 위해 결측치 무작위 생성

rows.to.missing <- sample(row.names(housing.df),10)#sample 10개 추출
housing.df[rows.to.missing,]$BEDROOMS <- NA#추출된 sample에 NA(Not available)값 삽입
summary(housing.df$BEDROOMS)#NA's:10

#결측치를 중앙값으로 변경
housing.df[rows.to.missing,]$BEDROOMS<-median(housing.df$BEDROOMS, na.rm = TRUE)#방 개수에는 소수점이 없기에 평균이 아닌 중앙값 사용
summary(housing.df$BEDROOMS)


##### 데이터 분할

#50%를 학습 데이터로 무작위 분할
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.5)#row 개수의 50%: 2901개
train.data <- housing.df[train.rows,]

#30%를 검증 데이터로 무작위 분할(검증 데이터: 다양한 모델 훈련시킨 뒤 고를 때때)
#setdiff(x,y): x에만 있고 y에는 없는 것 (X-Y 차집합)
valid.rows <- sample(setdiff(rownames(housing.df), train.rows),dim(housing.df)[1]*.3)#train데이터에서 없었던 데이터에서
#전체 데이터 크기의 30%를 추출
valid.data <- housing.df[valid.rows,]


#20%를 평가 데이터로 무작위 분할
#union(): 합집합 of two objects (중복 제거)
test.rows <- setdiff(rownames(housing.df), union(train.rows, valid.rows))
test.data <- housing.df[test.rows,]


##### 예측 사례

#학습 데이터를 이용한 학습 및 학습 성능 파악
#lm(target~input, data = data): linear model
#.: 나머지 모든 것
reg <- lm(TOTAL_VALUE ~ ., data = housing.df, subset = train.rows)#subset = train.rows: train row만 진행
tr.res <- data.frame(train.data$TOTAL_VALUE, reg$fitted.values, reg$residuals)#fitted.values: y_hat, residuals: y-y_hat

#검증 데이터를 이용한 예측 및 검증 성능 파악
pred <- predict(reg, newdata = valid.data)
vl.res <- data.frame(valid.data$TOTAL_VALUE, pred, residuals=valid.data$TOTAL_VALUE-pred)
head(vl.res)

#평가 데이터를 이용한 예측 및 평가 성능 파악
test <- predict(reg, newdata = test.data)
tt.res <- data.frame(test.data$TOTAL_VALUE, test, residuals=test.data$TOTAL_VALUE-test)
head(tt.res)




#Histogram
hist(housing.df$ROOMS)




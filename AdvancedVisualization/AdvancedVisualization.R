#2018312824 류채은
#인공지능통계론 실습과제결과물

# Exploratory Data Analysis

#utils 패키지: read.csv 
#readr 패키지: read_csv

exam <- read.csv("exam.csv")#csv 파일 읽어옴
exam

#file.choose(): 파일 다이얼로그 박스를 이용해서 원하는 파일을 선택할 수 있음
exam <- read.csv(file.choose())#변수 exam에 넣을 csv 파일 선택 가능
exam

class(exam)#data.frame
str(exam)#structure

#read_csv 함수(readr 패키지)
#install.packages("readr")
library(readr)#패키지 불러오기

exam <- read_csv("exam.csv")#어떻게 parsing했는지 알려줌

class(exam)
str(exam)


## 기초 통계 함수
summary(exam)#요약 통계량 산출하기

cor(exam)#변수간 상관관계 산출하기

hist(exam$english)#히스토그램 시각화하기
hist(exam$science)
hist(exam$class)

#어떤 변수의 막대그래프 시각화하기
library(ggplot2)
qplot(exam$english)

#박스플롯 시각화하기
boxplot(exam)#박스플롯은 다변량에서 많이 사용

#박스플롯의 값을 정확히 확인하기
boxplot(exam$science)$stats

#산점도를 매트릭스 형태로 시각화하기(graphics 패키지)
pairs(exam)

# 기술 통계 분석 (Descriptive Statistics)
# - 데이터를 요약해 설명하는 통계 기법
# - 성적을 집계해서 반별 과학 성적 평균을 구하는 것 등등

# 추론 통계(Inferential Statistics)
# - 어떤 값이 발생할 확률을 계산하는 통계 기법

#p-value가 0.05보다 크기 때문에 통계적으로 유의하지 않고 우연히 발생했을 가능성이 높다.
t.test(data=exam, science~class, var.equal = T)

#상관분석: 두 변수의 관계성 분석

cor.test(exam$class, exam$science)#상관관계가 0.5이하이므로상관관계가 별로 없음

#install.packages("corrplot")
library(corrplot)

exam_cor <- cor(exam)#먼저 상관관계 분석 
corrplot(exam_cor)
corrplot(exam_cor, method = "number")
corrplot(exam_cor,
         method = "color",#색깔로 표현
         type = "lower",#왼쪽 아래 행렬만 표시
         order = "hclust",#유사한 상관계수끼리 군집화
         addCoef.col = "black",#상관계수 색깔
         tl.col = "black",#변수명 색깔
         tl.srt = 45,#변수명 45도 기울임
         diag = F)#대각 행렬 제외

#데이터 시각화 ggplot2: 기본편

#+로 연결
#geom_line: 선
#geom_point: 점
#geom_bar: 막대
#geom_path: 경로

#1. ggplot 객체 생성(데이터 선택+심미적인 요소 명시)
#2. 기하학적 요소(ex. 점, 선, 막대, 경로 등) 추가
#3. 기타 다른 요소 추가

#Fares paid by passengers
#install.packages("titanic")
library(titanic)
data("titanic_train",package = "titanic")
titanic <- titanic_train#titanic data 불러오기

ggplot(data = titanic, aes(x=Fare))+geom_histogram()#Fare에 대한 히스토그램 시각화

# 다양한 방식으로 시각화 가능(ggplot2은 융통성 있음)

ggplot(data = titanic)+geom_histogram(aes(x=Fare))
ggplot()+geom_histogram(data = titanic, aes(x=Fare))
#install.packages("magrittr")
library(magrittr)
library(dplyr)
library(ggplot2)
titanic %>%
  ggplot()+
  geom_histogram(aes(x=Fare))
titanic %>%
  ggplot(aes(x=Fare))+
  geom_histogram()

ggplot(titanic, aes(x=Fare))+geom_histogram(bins=15)

#Relationships between Time and Passes in the worldcup 2010
#install.packages("faraway")
#install.packages('faraway', repos='http://cran.us.r-project.org')
# ‘C:\Users\LG\Documents\R\win-library\4.0/00LOCK’를 삭제해야 가능했음
library(faraway)

data(worldcup)
ggplot(worldcup, aes(x = Time, y = Passes))+geom_point()
ggplot(worldcup, aes(x = Time, y = Passes, color = Position, size = Shorts))+ geom_point()

#데이터시각화 ggplot2 확장편: 지오메트릭 함수 적용하기

#Using multiple geoms
noteworthy_players <- worldcup %>% filter(Shots == max(Shots) |
                                            Passes == max(Passes)) %>%
  mutate(point_label = paste(Team, Position, sep = ", "))

ggplot(worldcup, aes(x = Passes, y = Shots))+
  geom_point()+
  geom_text(data = noteworthy_players,
            aes(label = point_label),
            vjust = "inward",
            hjust = "inward")

ggplot(worldcup, aes(x = Time))+
  geom_histogram(binwidth = 10)+
  geom_vline(xintercept = 90*0:6,
             color = "blue", alpha = 0.5)

ggplot(worldcup, aes(x = Time, y = Passes))+geom_point(color = "darkgreen")

library(faraway)
data(nepali)

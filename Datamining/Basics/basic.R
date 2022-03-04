####### 기초 문법

#객체(변수)의 할당과 삭제
#실행: Ctrl + Enter

x = 13#할당
y<-sqrt(9)
4*x->z
k = 2


#자료 구조
x <- 2.7		# 실수
y <- "string"	# 문자열
z <- FALSE		# 논리값



#벡터
v <- c(1.2, 2.7, 3.1, 4.9, 5.4)
w <- c(TRUE, FALSE, FALSE)
x <- c("a", "b", "c", "d")
y <- 1.5:4.9		# 시작값:종료값:1씩 증가(1.5 2.5 3.5 4.5)
z <- seq(1,5)#1,2,3,4,5

#행렬
a<- matrix(
  c(1, 2, 4, 8, 16, 32),
  nrow=2,
  ncol=3
)



#산술 연산자
3 + 2
7 - 4
11 * 5
16 / 5
16 %/% 5		# 정수나눗셈 == 3
16 %% 5			# 나머지
3^2				# 거듭제곱

v <- c(1.2, 2.7, 3.1)
v <- v + 1		# 2.2 3.7 4.1

#조건 연산
x <- c(11,12,13,14,19,20)
x > 15
x[x>15]
which(x>15)#어디에 있는지 index(1부터 시작)로 알려줌

#조건문과 비교/논리 연산자
if(x == 3 || x >= 7 || x <= 1) {
  x <- x+1
}


#패키지의 설치와 사용
install.packages("ggplot2")#설치
library(ggplot2)#불러오기

#데이터프레임
x=c(1,2,3,4,5)
y=c(2,4,6,8,10)
z=c('M','F','M','F','M')
our.dataFrame <- data.frame(x,y,z)
colnames(our.dataFrame) <- c("number", "age", "gender")
rownames(our.dataFrame) <- c("a","b","c","d","e")

#데이터프레임의 구조 확인
str(our.dataFrame)
head(our.dataFrame)


#데이터프레임의 특정 열에 접근
our.dataFrame[1,]
our.dataFrame$z
our.dataFrame[our.dataFrame$geder=="F",]

#합/평균, 결측치 제거
sum(c(1,2,3))
sum(c(1,2,3,NA))
sum(c(1,2,3,NA),na.rm=T)#결측치 제거한 후 합
mean(c(1,2,3))
mean(c(1,2,3,NA))
mean(c(1,2,3,NA),na.rm=T)

#기초 시각화 도구
hist(c(5, 1, 5, 2, 4, 1, 5, 4, 5, 1, 1, 3))

plot(function(x){ 2*x^3 - 5*x }, xlim=range(-10,10))

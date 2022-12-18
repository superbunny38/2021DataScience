#Rstudio는 Linux 환경


##########변수 명 설정하기 & 변수 할당하기
#변수, 값 할당, 출력
abc <- 3
.jeong <- abc
#에러 남: 2.res <- 3
print(.jeong)#출력

.kai <- 0.0001


x<-1
print(x)

msg <- "hello"#문자열 할당
x<-5
x<-11:30#11부터 30까지 정수 할당

x[-1]
length(x[-1])#맨 앞에 원소만 빼고 출력
length(x)
x[length(x)]#마지막 값



######### 기본 명령어 (산술 연산)
x<--1;y<-3
x+y
x-y
x*y
x/y
x^y

5%/%2#5를 2로 나눈 몫
13%/%3#13을 3으로 나눈 몫
5%%2#5를 2로 나눈 나머지
5%%3#5를 3으로 나눈 나머지


#지수 표현 Numbers with exponents
#e3 = 10의 3승
1.2e3#1.2*1000=1200
.3e2#30
1.2e-3#0.0012

#복소수 Complex Numbers(Number 체계에서 가장 큰 수는 복소수)
x1<-1.2+3.4i
z1<-4i
z1+x1#연산

#수학 함수
x<-10;y<-3.21;n<-2
log(x)#자연 로그

log(4)
log(5)

log10(10)
log(3,2)
log(2,4)#log_4(2): 0.5
log(9,3)#log_3(9): 2

x
exp(x)#e^10
sin(x)
cos(x)
tan(x)
asin(-1)
acos(0.5)
atan(0.3)

abs(-1)
sqrt(4)#4^(1/2)
sqrt(9)

#floor: 내림, ceiling: 올림
floor(3.7)#3
ceiling(3.7)#4
ceiling(3.2)#4

round(0.0001,digits=0)
round(18.3125, digits = 0)#소수점부터 0개 숫자 표시
round(18.3125, digits = 1)#소수점부터 1개 숫자 표시
round(18.3125, digits = 2)#소수점부터 2개 숫자 표시
round(18.3128, digits = 3)#소수점부터 3개 숫자 표시


gamma(x)#gamma distribution
lgamma(x)
factorial(3)#3!

choose(3,2)#3C2
choose(4,2)#4C2






############## 벡터 Vector
#벡터: 같은 종류의 객체들만 포함하는 자료형태
x<-c(0.5,0.6)#numeric
x
x1<-c(TRUE,FALSE)#logical
x1

x+x1#TRUE = 1, FALSE = 0으로 환산되어 연산됨

q<-c("a","b","c")
q

a<-c(T,F)#logical: TRUE, FALSE
a

b <- 9:29#integer: 9~29
b

r <- c(1+0i, 2+4i)#complex
r

r1 <- c(1+0i:5+4i)#왜 6+4i까지 포함되지..?
r1

#벡터 생성하기: c()
d <- c(1,2.5,3.2)#double
d#1->1.0

t <- c(1L,2L,3L)#integer: 1,2,3
t

t1 <- c(1,2,3)
class(t1)

z <- c("KTX","Saemaul","Mugunghwa")#string
z

v <- c(TRUE, FALSE, FALSE, TRUE)#logical
v

#벡터에서의 인덱싱
x<-c(1:10)
x

x[3]#3번째 원소
x[c(1,3)]#첫번째 & 세번째 원소

fruit <- c(5,3,2)#5번째, 3번째, 2번째 원소
fruit

#벡터에 이름 부여하기
fruit <- c(1,2,3,4)
names(fruit)<-c("apple","orange","pineapple")
fruit
length(fruit)#4

fruit[c("apple", "pineapple")]#이름으로 인덱싱

#처음부터 이름짓기
fruit <- setNames(c(5,3,2),c("apple","orange","peach"))
fruit

length(fruit)#3

#기존 벡터를 사용하여 새로운 벡터 생성하기
a<-c(1,2,3)
b<-c(5,6)
x<-c(a,4,b)
x

#인덱스 범위가 넘는 자리에 값을 할당하면:
#NA: Not Available
a[7]<-4#1 2 3 NA NA NA 4
a
length(a)#7

a[2]<-90
a

#기존 벡터에 객체 삽입하기
append(a,88,after=2)#두번째 값 뒤에 88을 넣음
a#a<-append해야 실제로 삽입됨


append(x,-99,after=0)#맨 처음에 -99 삽입
x

#seq(), rep()을 사용하여 벡터 생성하기
#seq: sequence, rep: repeat
x<-seq(from = 0, to = 1, by = 0.1)#0부터 1까지 step value: 0.1
x

y<-seq(from = 0, to = 1, length= 11)#11개
y

y1<-seq(from = 0, to = 1, length= 10)#11개
y1

rep(1,10)#1을 10개 repeat: 1,1,1,1, ... ,1
length(rep(1,10))#10

#벡터 간 산술 연산하기
x<-1:3;y<-c(2,2,2)

x+y
x-y
x*y
x/y
x^y

#벡터에서 서로 다른 객체들을 혼용하면-> implicit coercion
#priority: Character > Numeric > Logical
#size 큰 순서대로 우선순위!

#implicit coercion
y<- c(1.7,"a")#1.7 -> "1.7"
y

y1 <- c(TRUE,2)#TRUE -> 1
y1

y2 <- c("a",TRUE)#TRUE -> "TRUE"
y2

#explicit coercion
x<-0:6
class(x)#integer

x<- as.numeric(x)
x
class(x)#numeric

x<-"0":"6"
x
class(x)#integer

as.logical(x)#FALSE, TRUE, ... , TRUE
x<- as.character(x)
class(x)#character

x<-as.logical(x)
x#NA,NA,...,NA
as.character(x)

x<-c("a","b","c")
x
as.numeric(x)#NA, NA, NA
as.logical(x)#NA, NA, NA
as.complex(x)#NA, NA, NA

#정규분포를 따르는 난수 발생시키기
x<-rnorm(10)#normal distribution
x

y <- 1:10
y

z <- -5:4
z

# 통계함수 사용하기
x
max(x)#최대값
min(x)#최소값
sum(x)#모든 성분의 합
prod(x)#모든 성분의 곱
mean(x)#평균
median(x)#중앙값
range(x)#최대값과 최솟값
quantile(x,probs=c(0.2,0.7))#분위수
var(x)#분산
sd(x)#표준편차
cov(x,y)#공분산
cor(x,y)#상관계수
cumsum(x)#누적합
cumprod(x)#누적곱
cummax(x)#누적최대값

x2 <- 1:10
cummax(x2)#누적최대값..?
cummin(x2)#누적최소값

x3 <- 11:20
x4 <- 21:30
pmax(x2,x3,x4)#성분별 최대값
pmin(x2,x3,x4)#성분별 최소값




#############매트릭스 (Matrices) 생성하기


#매트릭스 생성 방법1
m <- matrix(nrow = 2, ncol = 3)#NA 값으로 채워짐
m

m1 <- matrix(1, nrow = 2, ncol = 3)#1로 채음
m1

m+m1#1+NA=NA됨

#매트릭스 생성 방법2
m <- 1:10
dim(m)<-c(2,5)#row:2, column:5
m

#매트릭스 생성 방법3
#cbind: 두 개의 벡터를 column vector로 만들어서 binding
#rbind: 두 개의 벡터를 row vector로 만들어서 binding
x<-1:3
y<-10:12


cbind(x,y)
#1 10
#2 11
#3 12

rbind(x,y)
#1  2  3
#10 11 12

#다양한 방법으로 매트릭스 생성
하기
z <- matrix(1:20,4,5)# 4x5 행렬
z

z1 <- matrix(2,4,5)
z1

z2 <- matrix(c(1,2,3,4,5,6),nrow =2,ncol=3)
z2

z3 <- matrix(c(1,2,3,4,5,6),nrow =2,ncol=3,byrow = T)
#byrow=T: row-wise형태
#row 먼저 채움
z3

z3 <- matrix(c(1,2,3,4,5,6),nrow =2,ncol=3,byrow = FALSE)
#byrow=FALSE(default): column-wise형태
z3

y<-matrix(c(1,2,3,4),nrow=2,byrow=T)
#(1,1)->(1,2)->(2,1)->(2,2) 순서로 채움
y
y<-matrix(c(1,2,3,4),nrow=2)#default: byrow=FALSE
y

x<-1:4
y<-5:8

cbind(x,y)
#1 5
#2 6
#3 7
#4 8

rbind(x,y)
#1 2 3 4
#5 6 7 8


#기본 벡터 혹은 행렬을 결합해서 새로운 행렬을 생성하기
B <- matrix(0,4,5)
B#0을 채진 4x5 matrix

cbind(B,1:4)

A <- matrix(1:20,4,5)
B <- matrix(1:20,4,5)
C <- cbind(A,B)
C

#행렬간 산술연산
A+B
A-B
A*B
A/B

#행렬의 각 행과 열에 이름을 부여하기
z <- matrix(1:20,4,5)
z

colnames(z)<-c("alpha","beta","gamma","delta","eps")
z

rownames(z)<-c("a","b","c","d")
z

z["a","alpha"]#[row,column]
z[1,2]#첫 번째 row의 두 번째 값
z[4,]
z[,4]

#############리스트 생성하기(약간 class랑 비슷한 듯)
#리스트: 다른 객체들로 구성된 특별한 형태의 벡터
x <- list(1,"a",TRUE,1+4i)
x

x[2]# "a"

T#TRUE
 
Hong <- list(cor.name = "홍길동",
             eng.name = "Gil-dong",
             married = T,
             no.child = 2,
             child.ages = c(13,10)
             )
Hong

#str: 자료형태의 구조를 알려줌
str(Hong)#str: structure

#리스트 성분 인덱싱 방법1:
# $: 접근법
Hong$child.ages
Hong$cor.name#이렇게도 빼올 수 있음
Hong$no.child <- 3#바뀜
Hong

Hong$child.ages[2]#두 번째 값

#리스트 성분 인덱싱 방법2:
Hong[1]#$cor.name "홍길동"
Hong[[1]]#"홍길동"
Hong[length(Hong)]#Hong$child.ages
Hong[c(1,2)]
Hong[5]#$child.ages 13 10
Hong[5][1]#$child.ages 13 10
Hong[5][2]#NULL
Hong[[5]][1]#13

#에러남: Hong[[c(1,2)]]


x <- list(a=1:10,
          beta = exp(-3:3),
          logic = c(TRUE, FALSE, FALSE, TRUE))


x<-list(a=1:10,
        beta = exp(-3:3),
        logic = c(T, F, F, T))
x$a

x$beta

x$n <- c(0,10)
x

length(x$beta)#7

x$logic

#lapply: 각 성분에 대해 같은 함수를 반복적으로 적용한 값을 리턴하기
#lapply: apply a function over a function to a list or vector
x

#1:3/4 == c(1,2,3)/4 -> 0.25, 0.5, 0.75
lapply(x,quantile, probs = (1:3)/4)#4등분
lapply(x,quantile, probs = (1:4)/5)#5등분








###############범주형 데이터 (Factor)
#"self-describing"
#범주형 데이터 생성하기

x<- factor(c("yes","yes","no","yes","no"))
x#levels are put in alphabetical order(levels = no yes) no 레벨이 더 낮음

#the order can be set using "levels"
x<- factor(c("yes","yes","no","yes","no"), levels = c("yes","no"))
x#yes: level1, no: level2

#table() 함수: 도수분포표를 작성해주는 함수

blood.type <- factor(c("A","A","AB","O","O"), levels = c("A","B","AB","O"))

table(blood.type)#A: 2개, B: 0개, AB: 1개, O: 2개

blood.type1 <- factor(c("A","A","AB","O","O"))
table(blood.type1)#level 지정 안 했을 때







#################데이터 프레임 (Data Frame)
#Data Frame: 같은 길이의 벡터들을 성분으로 갖는 리스트

#데이터 프레임 생성하기
#pandas df 처럼 나옴
x<- data.frame(id = 1:4, name = c("Kim","Lee","Park","Choi"))
x

x <- c(100,75,80)
y <- c("A302043","A302044","A302045")
z <- data.frame(score=x, ID = y)
z

d <- data.frame(x = 1:2, y = c("ab","bc"))
d
str(d)#자동으로 범주형 데이터로 안 바뀌는데요...?

dat.1 <- data.frame(x=1:3, y = c('a','b','c'))
str(dat.1)

# strings are read by default as factors 
dat.2 <- data.frame(x=1:3, y = c('a','b','c'), stringsAsFactors = F)
dat.2
str(dat.2)

dat.3 <- data.frame(x=1:3, y = c('a','b','c'), stringsAsFactors = T)
dat.3
str(dat.3)

dat.2 == dat.3#True

dat.4 <- data.frame(x=1:3, y = c('a','b','c'))
dat.4#하지만 여기선 y = factors로 안 읽힘..
str(dat.4)


#여러 개의 데이터프레임을 행방향으로 붙여 새로운 데이터프레임 생성하기(열 방향 아닌강..?)---->라서 행방향인가?
#행: row, 열: column

a <- data.frame(x = c(5,10,15),y = c("a","b","c"))
b <- data.frame(z = c(10,20,30))

a
b

cbind(a,b)

#여러 개의 데이터프레임을 열방향으로 붙여 새로운 데이터프레임 생성하기
a1 <- data.frame(x=c(20,25,30), y = c("d","e","f"))
a
a1
rbind(a,a1)

#만약 column이름이 다르면?
a2 <- data.frame(y = c("1","2","4"), z= c("a",5,"2"))
a1
a2
# 에러 남: rbind(a1,a2)
 
 
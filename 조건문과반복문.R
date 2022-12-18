# chap04_1_control

# 제어문 : 조건문, 반복문

## <실습> 산술연산자 
num1 <- 100 # 피연산자1
num2 <- 20  # 피연산자2
result <- num1 + num2 # 덧셈
result # 120
result <- num1 - num2 # 뺄셈
result # 80
result <- num1 * num2 # 곱셈
result # 2000
result <- num1 / num2 # 나눗셈
result # 5

result <- num1 %% num2 # 나머지 계산
result # 0

result <- num1^2 # 제곱 계산(num1 ** 2)
result # 10000
result <- num1^num2 # 100의 20승
result # 1e+40 -> 1 * 10의 40승과 동일한 결과


# <실습> 관계연산자 
# (1) 동등비교 
boolean <- num1 == num2 # 두 변수의 값이 같은지 비교
boolean # FALSE
boolean <- num1 != num2 # 두 변수의 값이 다른지 비교
boolean # TRUE

# (2) 크기비교 
boolean <- num1 > num2 # num1값이 큰지 비교
boolean # TRUE
boolean <- num1 >= num2 # num1값이 크거나 같은지 비교 
boolean # TRUE
boolean <- num1 < num2 # num2 이 큰지 비교
boolean # FALSE
boolean <- num1 <= num2 # num2 이 크거나 같은지 비교
boolean # FALSE

# <실습> 논리연산자(and, or, not, xor)
logical <- num1 >= 50 & num2 <=10 # 두 관계식이 같은지 판단 
logical # FALSE
logical <- num1 >= 50 | num2 <=10 # 두 관계식 중 하나라도 같은지 판단
logical # TRUE

logical <- num1 >= 50 # 관계식 판단
logical # TRUE
logical <- !(num1 >= 50) # 괄호 안의 관계식 판단 결과에 대한 부정
logical # FALSE

x <- TRUE; y <- FALSE
xor(x,y) # [1] TRUE
x <- TRUE; y <- TRUE
xor(x,y) # FALSE#

# 1. 조건문 : if(), ifelse(), which()

#   1) if(조건식)
x <- 10
y <- 5

z <- x * y
z # 50

# 조건식 : 산술, 관계, 논리, 연산자 이용

## 형식(1)
if (z >= 50){
  cat('z는 50 보다 크다.')
}else{  # no
  cat('z는 50보다 작다.')
  }


## 형식(2)
score <- scan()  # 점수입력
score  # 점수 -> 등급(grade)

grade <- ''  # 빈 변수 정의

if(score >= 90 & score <= 100){
  grade <- 'A학점'
}else if(score >= 80){
  grade <- 'B학점'
}else if(score >= 70){
  grade <- 'C학점'
}else{
  grade <- 'F학점'
}

cat('성적 : ', score, ', 등급 : ', grade)


# 문제) 키보드로 임의 숫자를 입력 받아서 짝수 or 홀수를 판별하라
input <- scan()  # 숫자 입력
input

if(input %% 2 ==0){  # 산술 > 관계
  cat("입력한 수는 짝수")
}else{
  cat("입력한 수는 홀수")
}


#   2) ifelse() : if + 반복
score <- c(78,85,65,90,68)
# vector(5) -> ifelse() -> vector(5)

# ifelse(test, yes, no)
result <- ifelse(score >= 70, "합격", "불합격")
result

# 결측치 처리 : 평균 대체
score2 <- c(78, 85, NA, 90, 68)

result2 <- ifelse(is.na(score2), mean(score2, na.rm = TRUE), score2)
result2


#   3) which문 : 해당 값의 위치(index)반환
names <- c("kim", "choi", "lee", "park")

idx <- which(names == 'park')

names[idx]



# 2. 반복문 : for()), while()

#   1) for(변수 in 값){실행문}
num <- 10:20
num; length(num)  # 11

d <- numeric()  # vector형 변수 선언

idx <- 1  # index
for(i in num){  # 11회 반복
  cat('i = ', i, "\n")  # "\n" : 줄 바꿈
  print(i * 2)
  d[idx] = i * 2 #d[1]  <-20
  isx <- idx + 1
  }


d # 20 22 24 26 28 30 32 34 36 38 40

# for + if
for(i in num){  # 11회 반복
  if(i %% 2 == 0)  # 5의 배수 출력
    print(i)
}


# 문제2) 키보드로 5개 숫자를 입력받아서 짝수만 출력하기
s_num <- scan()

even <- 0
odd <- 0

for(i in s_num){
  if(i %% 2 == 0)
    even <- even +i  # 짞수 누적 변수
  else
    odd <- odd + i  # 홀수 누적 변수    
}


cat("짝수합 = ", even, ", 홀수합 = ", odd)



#   2) while(조건문){실행문}
i = 0  # 초기화
s = 0

while(i < 10){
  i <- i + 1
  s <- s + i  # 누적 변수
  cat("i = ", i, ", s = ", s, "\n")
}


# 파생 변수 추가
getwd()
setwd("C:/")

emp <- read.csv('emp.csv')
emp

# 평균 이상 급여 수령자 유무 컬럼 추가
avg <- mean(emp$pay)
avg  #370

# 방법 1) ifelse 이용
emp$result <- ifelse(emp$pay > avg, "이상", "미만")   # ifelse(조거식, 참, 거짓)
emp

# 방법 1) for + if 이용
result <- character()  # 빈 vector
for(i in 1:length(emp$no)){
  result[i] <- "이상"
}else{
  result[i] <- "미만"
}


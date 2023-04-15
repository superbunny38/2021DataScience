dt <- read.csv("lda_ex.csv",header = TRUE, sep = ',')
dt
dt <- dt[,-1]#object삭제
dt
dt.1 <- dt[dt$class == 1,]
dt.2 <- dt[dt$class == 2,]
dt.1

mu.1 <- colMeans(dt.1[,-3])
mu.2 <- colMeans(dt.2[,-3])

sigma.1 <- var(dt.1[,-3])
sigma.1

sigma.2 <- var(dt.2[,-3])
sigma.2

#pooled variance-covariance matrix for LDA
sigma.p <- ((4-1)*sigma.1+(5-1)*sigma.2)/(4+5-2)

#multivariate pdf 구하기 (x에서의 pdf를 반환하는 함수)
mymvdnorm <- function(x,mu,sigma){#x: vector
  p <- length(x)#차원
  x <- matrix(as.numeric(x),p,1)#px1짜리 matrix로 변환
  mu <- matrix(as.numeric(mu),p,1)#matrix 형태(px1)로 바꿔줌
  sigma <- as.matrix(sigma)#sigma를 matrix 포맷으로 변형
  
  #multivariate normal distribution dfv
  #det: determinant matrix
  
  #denominator: d
  d <- (2*pi)^(p/2)*sqrt(det(sigma))#pi: 3.14xxx 
  #numerator: n
  n <- exp(-.5*t(x-mu)%*%solve(sigma)%*%(x-mu))
  dfv <- n/d
  return(dfv)
}

mymvdnorm(c(3,3),mu.1,sigma.1)#pdf 반환됨

dt

#LDA로 예측
pred.y <- rep(0,nrow(dt))#initialize prediction with 0
pred.y
for (i in 1:nrow(dt)){
  f1<- mymvdnorm(dt[i,-3],mu.1,sigma.p)
  f2 <-mymvdnorm(dt[i,-3],mu.2,sigma.p)
  
  if (f1>=f2){
    pred.y[i] <- 1
  }else{
    pred.y[i] <- 2
  }
  
}

pred.y
table(dt$class,pred.y)#9개 중 7개 맞춤

#QDA로 예측
pred.y <- rep(0,nrow(dt))#initialize prediction with 0
pred.y
for (i in 1:nrow(dt)){
  f1<- mymvdnorm(dt[i,-3],mu.1,sigma.1)
  f2 <-mymvdnorm(dt[i,-3],mu.2,sigma.2)
  
  if (f1>=f2){
    pred.y[i] <- 1
  }else{
    pred.y[i] <- 2
  }
  
}

pred.y
table(dt$class,pred.y)#모두 정분류됨

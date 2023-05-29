dt.price <- read.csv("price.csv", header = TRUE, sep = ",")
head(dt.price)

n <- nrow(dt.price)#행 개수
p <- ncol(dt.price)#열 개수

mtdt.price <- dt.price - matrix(rep(colMeans(dt.price),n),n,p,2)
mtdt.price#mean centered 된 데이터 

colMeans(mtdt.price)#feature마다 0에 평균이 가까운 것 확인
 
#variance-covariance matrix 구하기
var(mtdt.price)#1
var(dt.price)#2

#데이터의 중심(평균)만 이동했기에 1과 2가 동일

s <- var(mtdt.price)
total.variance <- sum(diag(s))
total.variance#원래 데이터의 variance의 합

#Spectral Decomposition
dt.eig <- eigen(s)
dt.eig#eigen vector와 eigen value 둘다 산출

plot(dt.eig$values, type = "o", col = "blue", xlab = "number of PCs", ylab = "eigen value")

total.variance.2 <- sum(dt.eig$values)#eigen value 합
total.variance.2#original data의 variance의 합이 eignen value의 합과 동일
  
proportion <- dt.eig$values/total.variance.2
proportion

#교수님께선 이 부분 코딩 안 함
cumulative.prop <- c(proportion[1],
                     sum(proportion[1:2]),
                     sum(proportion[1:3]),
                     sum(proportion[1:4]),
                     sum(proportion[1:5]))
#몇개까지 썼을 때 어느 비율까지 표현 가능한지
cumulative.prop

#새로운 좌표
pc.scores <- as.matrix(mtdt.price)%*% dt.eig$vectors
pc.scores

#correlation == 0 (off-diagonal 거의 0)
var(pc.scores)#diag이 eigen value외 굉장히 유사한 것 확인 가능

#PC 두 개 가지고 viz
plot(pc.scores[,1:2],xlab = "PC1","ylab" = "PC2")
text(pc.scores[,1:2]+1, labels =row.names(pc.scores))#도시 이름 시각화 



##
-dt.eig$vectors#eigen vector에 -1곱함
dt.eig$vectors

pc.scores <- as.matrix(mtdt.price)%*% (-dt.eig$values)
pc.scores#교안대로 안 나옴
 
#generate artificial data
#rnorm: 정규분포로부터 random하게 100개 generate
#sd: 표준편차
#정해주지 않으면 평균은 0
x<-rbind(matrix(rnorm(100,sd = 0.3),ncol = 2),matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
colnames(x) <- c("x","y")

cl <- kmeans(x,2) 
cl
cl$iter#한 번만에 완료

cl$tot.withinss
kmeans(x,1)$withinss#하나로 묶었을 때


plot(cl$centers,col = 1:2, pch = 8, cex = 2)

kmeans(dt.cars,2,nstart = 3)#n start: multiple starts
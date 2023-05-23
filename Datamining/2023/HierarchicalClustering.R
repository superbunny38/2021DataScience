#Hierarchical Clustering

dt.cars <- read.csv("cars.csv",header = TRUE, sep = ",")
dt.cars

#scaling
dt.cars.scaled <- scale(dt.cars, center = TRUE, scale = TRUE)
colMeans(dt.cars.scaled)#scaling이후 평균이 0에 거의 가까운 것 확인
apply(dt.cars.scaled,2,sd)#표준퍈치도 1로 됨
dt.cars.scaled

#distance 계산
d.mat <- dist(dt.cars.scaled, method = "euclidean")
d.mat#symmetric matrix


hc.cars <- hclust(d.mat, "ward.D")
plot(hc.cars)#dendogram 그려줌

#Final Cluster 만들어줌
clust.member <- cutree(hc.cars, k = 2)#dendogram 보고나서 클러스터 개수 k 정하면 좋음
clust.member

#각 feature에 대한 군집의 해석이 가능함
dt.cars$clust <- clust.member
boxplot(cc ~ clust, data =  dt.cars)
boxplot(cc ~ clust, data =  dt.cars, main = "cc", xlab = "cluster")
boxplot(ps.p.rpm ~ clust, data = dt.cars, main = "ps.p.rpm",xlab = "cluster")
boxplot(kgm.p.rpm ~ clust, data = dt.cars, main = "kgm.p.rpm",xlab = "cluster")
boxplot(kmpl ~ clust, data = dt.cars, main = "kmpl", xlab = "cluster")

#cluster 1에 대한 statistics 출력
summary(subset(dt.cars,dt.cars$clust == 1))

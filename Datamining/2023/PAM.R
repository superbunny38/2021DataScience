#Partitioning Around Medoids (PAM)

install.packages("cluster")
library(cluster)

# Generate artificial data
# :25 objects into 2 clusters
#rnorm(10개,평균0,표준편차0.5)
x<- rbind(cbind(rnorm(10,0,0.5),rnorm(10,0,0.5)),
          cbind(rnorm(15,5,0.5),rnorm(15,5,0.5)))
x
pamx <- pam(x,2)
pamx

#cluster plotting
clusplot(pamx)

plot(pamx)

#apply to cars data
dt.cars <- read.csv("cars.csv",header = TRUE, sep = ",")
dt2 <- scale(dt.cars, center = TRUE, scale = TRUE)
dt2

#check if scaled well
colSums(dt2)#avg == 0
apply(dt2,2,sd)#std == 1

pam.cars <- pam(dt2,4)#4개 cluster
clusplot(pam.cars)

d.gograd <- read.csv("gograd.csv", header = TRUE, sep = ",")
head(d.gograd)

library(VGAM)
summary(d.gograd$apply)
#ordinal logistic regression
ologr <- vglm(apply ~ pared + public + gpa, cumulative(parallel = TRUE, reverse = FALSE),d.gograd)
summary(ologr)
#?vglm == help(vglm)

#로짓값 예측: logit(P(Y<=1)),logit(P(Y<=2))
predict(ologr,d.gograd[,-1])

#확률값 예측: P1,P2,P3
predict(ologr,d.gograd[,-1],type="response")

pred <- predict(ologr,d.gograd[,-1],type="response")
rowSums(pred)#확률 잘 나온지 확인 (sum해서 1로 되어야 함)

#class 예측
pred_class <- apply(pred,1,which.max)#row-wise

table(d.gograd[,1],pred_class)#[0,0]: 1번 class인데 1로 예측한 개수 [1,1]: 2번 클래스인데 2로 예측한 

unique(pred_class)#예측된 class

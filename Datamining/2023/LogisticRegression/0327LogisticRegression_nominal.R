
d.brand <- read.csv("brandchoice.csv",header = TRUE, sep = ",")

#p.40
head(d.brand)
table(d.brand$brand)
table(d.brand$female)
summary(d.brand$age)
xtabs(~ d.brand$female+d.brand$brand)

#p.41
library(mlogit)#nominal regression 하기 위해 필요
str(d.brand)#데이터 structure 확인: 수치형
d.brand$brand <- as.factor(d.brand$brand)#nominal로 변경
td.brand <- mlogit.data(d.brand,varying = NULL, choice = "brand",shape = "wide")#mlogit을 활용하기 위해 선언 필요

td.brand#데이터가 바뀐 것을 확인 가능

##########nlogr 설명
#independent variables:female,age
#dependent variable: brand
#baseline category: 1
nlogr <- mlogit(brand~1|female+age, data = td.brand, reflevel = "1")
summary(nlogr)






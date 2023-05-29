#Associate Rule Mining
install.packages("arules")
library("arules")

d.groc <- read.csv("groceries.csv",header = TRUE, sep = ",")
dim(d.groc)

d.groc <- as.matrix(d.groc)#data frame을 matrix로 변환
iM.groc <- as(d.groc, "itemMatrix")#itemMatrix로 matrix를 변환

summary(iM.groc)#몇 번 나타나는지 등 보여줌

#Rule Generation

#supp: minimum support
#conf: minimum confidence
#target: rules : rules를 generate해줌
rules <- apriori(iM.groc, parameter = list(supp = 0.01, conf = 0.5, target = "rules"))
inspect(rules)

rules <- apriori(iM.groc, parameter = list(supp = 0.1, target = "frequent itemsets"))
inspect(rules)

#rules를 sorting 할 수도 있음



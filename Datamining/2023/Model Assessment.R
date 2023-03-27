d.c <- read.csv("cancer.csv",header = TRUE,sep = ",")
table(d.c$y)#target class dist. check
head(d.c)
str(d.c)#structure check

#Stratified Random Partition
#: Preserve the original class distribution

#benign인 데이터만 고름
d.c.ben <- d.c[d.c$y == "benign",]
#malignant인 데이터만 고름
d.c.mal <- d.c[d.c$y == "malignant",]

nr.ben <- nrow(d.c.ben)#444
nr.mal <- nrow(d.c.mal)#239

#shuffle
d.c.ben <- d.c.ben[sample(nr.ben),]
d.c.mal <- d.c.mal[sample(nr.mal),]

#stratified partition
d.c.ben.train <- d.c.ben[1:round(nr.ben*0.7),]
d.c.ben.test <- d.c.ben[round(nr.ben*0.7)+1:nr.ben,]

d.c.mal.train <- d.c.mal[1:round(nr.mal*0.7),]
d.c.mal.test <- d.c.mal[round(nr.mal*0.7)+1:nr.mal,]

d.c.train <- rbind(d.c.ben.train,d.c.mal.train)
d.c.test <- rbind(d.c.ben.test,d.c.mal.test)

table(d.c.train$y)
table(d.c.test$y)

#encode target variable y to fit model
d.c.train$y <- replace(d.c.train$y,d.c.train$y == "malignant",0)
d.c.train$y <- replace(d.c.train$y,d.c.train$y == "benign",1)
d.c.train$y <- as.numeric(d.c.train$y)
d.c.test$y <- replace(d.c.test$y,d.c.test$y == "malignant",0)
d.c.test$y <- replace(d.c.test$y,d.c.test$y == "benign",1)
d.c.test$y <- as.numeric(d.c.test$y)

#logistic regression
logr <- glm(y~.,data = d.c.train, family = binomial("logit"))

summary(logr)

pred.y.train <- round(predict(logr, newdata = d.c.train, type = "response"))
cm.train <- table(d.c.train$y, pred.y.train)
cm.train

pred.y.test <- round(predict(logr, newdata = d.c.test, type = "response"))
cm.test <- table(d.c.test$y, pred.y.test)
cm.test

#train acc
acc.train <- sum(diag(cm.train))/sum(cm.train)
acc.train

#test acc
acc.test <- sum(diag(cm.test))/sum(cm.test)
acc.test

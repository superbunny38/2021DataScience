#Confusion Matrices based on cutoffs of 0.5, 0.25, and 0.75 (Riding Mowers Example)

owner.df <- read.csv("ownerExample.csv")
head(owner.df)
dim(owner.df)
str(owner.df)
summary(owner.df$Class)
library(caret)

##cutoff = 0.5
pred <- ifelse(owner.df$Probability>0.5,"owner","nonowner")
pred <- as.factor(pred)#이렇게 level을 줘야 됨
owner.df$Class <- as.factor(owner.df$Class)#level을 줘야 됨
confusionMatrix(pred,owner.df$Class)#Acc: 0.875

##cutoff = 0.25
pred.25 <- ifelse(owner.df$Probability>0.25,"owner","nonowner")
pred.25 <- as.factor(pred.25)#이렇게 level을 줘야 됨
confusionMatrix(pred.25,owner.df$Class)#Acc: 0.791

##cutoff = 0.75
pred.75 <- ifelse(owner.df$Probability>0.75,"owner","nonowner")
pred.75 <- as.factor(pred.75)#이렇게 level을 줘야 됨
confusionMatrix(pred.75,owner.df$Class)#Acc: #Acc: 0.75





#Plot accuracy and overall error as a function of the cutoff value

##create empty accuracy table
accT = c()

#compute accuracy per cutoff
for( cut in seq(0,1,0.1)){#stepvalue:0.1
  pred <- ifelse(owner.df$Probability>cut, "owner", "nonowner")
  pred <- as.factor(pred)
  cm <- confusionMatrix(pred, owner.df$Class)
  accT <- c(accT, cm$overall[1])
  cat(cm$overall[1], " ")
}

accT
#plot accuracy
plot(accT ~ seq(0,1,0.1),xlab="Cutoff Value", ylab ="", type = "l", ylim = c(0,1))
lines(1-accT~seq(0,1,0.1),type="l",lty=2)
legend("topright", c("accuracy","overall error"),lty = c(1,2),merge = TRUE)



#ROC curve for riding mowers example
library(pROC)
r<-roc(owner.df$Class, owner.df$Probability)
plot.roc(r)

#compute AUC
auc(r)

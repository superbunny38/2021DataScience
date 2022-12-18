## Load and partition the riding mower data, and plot scatter plot

mower.df <- read.csv("RidingMowers.csv")
set.seed(11)
train.index <- sample(row.names(mower.df),0.6*dim(mower.df)[1])
valid.index <- sample(row.names(mower.df),train.index)
train.df <- mower.df[train.index,]
valid.df <- mower.df[valid.index,]

#new household
new.df <- data.frame(Income = 60, Lot_Size = 20)

##scatter plot
plot(Lot_Size ~ Income, data = train.df, pch = ifelse(train.df$Ownership ==  "Owner",1,3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos = 4)
text(60,20,"X")
legend("topright",c("owner","non-owner","newhousehold"),pch = c(1,3,4))


## Normalize data and find nearest neighbors

#initialize data and find nearest neighbors
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

#use preProcess() from the caret package to normalize Income and Lot_Size
library(caret)
norm.values <- preProcess(train.df[,1:2], method = c("center","scale"))

#predict(model, to where)
train.norm.df[,1:2] <- predict(norm.values, train.df[,1:2])
valid.norm.df[,1:2] <- predict(norm.values, valid.df[,1:2])
mower.norm.df[,1:2] <- predict(norm.values, mower.df[,1:2])
new.norm.df <- predict(norm.values, new.df)


##scatter plot normalized data
plot(Lot_Size ~ Income, data = train.norm.df, pch = ifelse(train.norm.df$Ownership ==  "Owner",1,3))
text(train.norm.df$Income, train.norm.df$Lot_Size, rownames(train.norm.df), pos = 4)
text(new.norm.df$Income,new.norm.df$Lot_Size,"X")
legend("topright",c("owner","non-owner","newhousehold"),pch = c(1,3,4))


# use knn() to compute knn
# knn() is available in library FNN (provides a list of the nearest neighbors)
# and library class(allows a numerical output variable)

library(FNN)
nn <- knn(train = train.norm.df[,1:2],
          test = new.norm.df,
          cl = train.norm.df[,3],
          k = 3)
head(train.norm.df)#index가 순서대로 되어있지 않음을 알 수 있음
row.names(train.df)[attr(nn, "nn.index")]
row.names(train.norm.df)[attr(nn, "nn.index")]
attr(nn, "nn.index")#k nearest points index
attr(nn, "nn.dist")#거리

summary(nn)#결과
nn

#initialize a data frame with two columns: k, and accuracy
accuracy.df <- data.frame(k = seq(1,14,1), accuracy = rep(0,14))
head(accuracy.df)
dim(accuracy.df)

#compute knn for different k on validation

# 안됨
# for(i in 1:14){
#   knn.pred <- knn(train.norm.df[,1:2], valid.norm.df[,1:2], cl = train.norm.df$Ownership, k = i)
#   accuracy.df[i,2] = confusionMatrix(knn.pred, as.factor(valid.norm.df$Ownership))$Overall[1]
#   #cat(confusionMatrix(knn.pred, valid.norm.df[,3]))
# }

accuracy.df <- data.frame(accuracy = rep(0,14))
accuracy.df

for(i in 1:14){
  knn.pred <- knn(train.norm.df[,1:2], valid.norm.df[,1:2], cl = train.norm.df[,3], k = i)
  # Factor: as.factor(valid.norm.df%OWnership)
  accuracy.df$accuracy[i] <- confusionMatrix(knn.pred, as.factor(valid.norm.df$Ownership))$overall[1]
}

knn.pred
accuracy.df#k=10

#Run the K-NN algorithm to classify the new household
knn.pred.new <- knn(mower.norm.df[,1:2],new.norm.df, cl = mower.norm.df$Ownership, k = 10)
knn.pred.new
row.names(train.df)[attr(nn,"nn.index")]

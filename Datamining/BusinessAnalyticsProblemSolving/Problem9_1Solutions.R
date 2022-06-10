

################## 
#      9.1       #
#   2018312824   #
#    류채은      #
##################

rm(list=ls())#reset environment
original.df <- read.csv("eBayAuctions.csv")
colnames(original.df)
head(original.df)
dim(original.df)

#Data Preprocessing
original.df$Duration <- as.factor(original.df$Duration)#convert Duration to categorical values
original.df$Competitive. <- as.factor(original.df$Competitive.)

#Data Partitioning (60% training data, 40% validation data)
set.seed(38)
train.index <- sample(c(1:dim(original.df)[1]), dim(original.df)[1]*0.6)
train.df <- original.df[train.index,]
dim(train.df)
valid.df <- original.df[-train.index,]
dim(valid.df)

###### a.
library(rpart)
library(rpart.plot)

#build classification tree(minbucket = 50, maxdepth = 7)

optimized.tree <- rpart(Competitive. ~ .,
                        data = train.df,
                        control = rpart.control(minbucket = 50, maxdepth = 7),
                        method = "class")
optimized.tree
#display built tree
prp(optimized.tree, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

##### b.
#predict on a validation dataset
predict.valid <- predict(optimized.tree, valid.df, type = "class")
confusionMatrix(predict.valid, valid.df$Competitive.)

##### c.: 레포트에 작성

##### d.
colnames(train.df)
colnames(valid.df)

#Close Price 제거
new.train.df <- train.df[,-6]
new.valid.df <- valid.df[,-6]

cat(sampled <- sample(x=1:100,size=1))
set.seed(sampled)
#train tree classifier
new.tree <- rpart(Competitive ~ ., data = train.df, method = "class" , xval = 5,minbucket = 50,maxdepth = 7)

#optimal pruning by optimal cp value
pruned.tree <- prune(new.tree, cp = new.tree$cptable[which.min(new.tree$cptable[,"xerror"]),"CP"])

#visualize pruned tree
prp(pruned.tree, type = 1, extra = 1, split.font = 2, varlen = -10)

#### e.
visualize.df <- original.df
visualize.df <- visualize.df[,-6]#eliminate ClosePrice
predicted <- predict(pruned.tree, visualize.df,type="class")#predict with the pruned tree
visualize.df$predicted <- predicted
visualize.df$predicted.label <- ifelse(visualize.df$predicted==1,'Competitive','Not Competitive')

library(ggplot2)#library for visualization
ggplot(visualize.df, aes(x = OpenPrice, y = Category, color = predicted.label))+
  geom_point(alpha=0.7)#더 나은 시각화를 위해 투명성 추가

#better visualization with continuous variable
visualize.valid <- new.valid.df
predicted.valid <- predict(pruned.tree, visualize.valid, type = "class")#use validation dataset
visualize.valid$predicted <- predicted.valid
visualize.valid$predicted.label <- ifelse(visualize.valid$predicted == 1,'Competitive','Not Competitive')

ggplot(visualize.valid, aes(x = OpenPrice, y = sellerRating, color = predicted.label))+
  geom_point(alpha=0.5,size=2)+#더 나은 시각화를 위해 투명성 추가
  xlim(0, 50)+
  ylim(0,9000)+
  geom_hline(yintercept=2969,linetype="dashed",color="blue",size=0.7)+#구분선(sellerRating==2969)
  geom_hline(yintercept = 601,linetype="dashed",color="blue",size=0.7)+#구분선(sellerRating==601)
  geom_vline(xintercept=1.8,linetype="dashed",color="red",size=0.7)+#구분선(OpenPrice==1.8)
  labs(x = "OpenPrice",y="sellerRating",size = 15)+
  theme(
    axis.title.y = element_text(size = 16),
    axis.title.x = element_text(size=16),
    legend.title = element_text(size=13),
    legend.text = element_text(size = 10))


######f

#분류표
confusionMatrix(predicted.valid, valid.df$Competitive.)

#liftchart
library(gains)
gain <- gains(as.numeric(valid.df$Competitive.), as.numeric(predicted.valid), groups = dim(valid.df)[1])
plot(c(0,gain$cume.pct.of.total*sum(as.numeric(valid.df$Competitive.)))~c(0,gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", main = "",type = "l")
lines(c(0,sum(as.numeric(valid.df$Competitive.)))~c(0,dim(valid.df)[1]),col="red",lty=2,size=3)


######g
seller.variable.importance <- as.data.frame(pruned.tree$variable.importance)
seller.variable.importance



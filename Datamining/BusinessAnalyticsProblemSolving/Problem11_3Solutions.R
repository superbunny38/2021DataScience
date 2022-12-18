

################## 
#      11.3      #
#   2018312824   #
#    류채은      #
##################

rm(list=ls())#reset environment
#install.packages("neuralnet")
library(neuralnet)

#read data
original.df <- read.csv("ToyotaCorolla.csv")

#variables to use
use.var <- c("Price", "Age_08_04", "KM", "Fuel_Type", "HP", "Automatic", "Doors", "Quarterly_Tax","Mfr_Guarantee", "Guarantee_Period","Airco", "Automatic_airco", "CD_Player", "Powered_Windows", "Sport_Model", "Tow_Bar")
length(use.var)

df <- original.df[,use.var]
sum(is.na(df))#check NA
summary(df)

#convert categorical variables to dummies
library(fastDummies)
summary(as.factor(df$Fuel_Type))
preprocessed.df <- dummy_cols(df,select_columns = c("Fuel_Type"))
colnames(preprocessed.df)
#다중 공선성이 생기지 않도록 dummy 3개 중 하나도 같이 없앤다
preprocessed.df <- preprocessed.df[,-c(4,17)]

sum(is.na(preprocessed.df))

#scale numeric variables
normalized.df <- preProcess(preprocessed.df, method = c("range"), na.remove = TRUE)
normalized.df <- predict(normalized.df, as.data.frame(preprocessed.df))

###### a.

#partition dataset (80% training, 20% validation)
train.index <- sample(row.names(normalized.df),0.8*dim(normalized.df)[1])
valid.index <- setdiff(row.names(normalized.df),train.index)
train.df <- normalized.df[train.index,]
valid.df <- normalized.df[valid.index,]

dim(train.df)
dim(valid.df)

#train neural network
a.nn <- neuralnet(Price ~ ., data = train.df, hidden = 2)
plot(a.nn, rep = "best")

#install.packages("Metrics") for rms error
library(Metrics)

#predict on a train dataset
t.pred <- compute(a.nn, train.df)$net.result
cat(train.error <- rmse(train.df$Price,t.pred))

#predict on a validation dataset
pred <- compute(a.nn, valid.df)$net.result
cat(valid.error <- rmse(valid.df$Price, pred))

#draw barplot to visualize rms error for train & validation dataset
error <- c(train.error, valid.error)
bar.df <- data.frame(type = c("train", "validation"), error = error)
p <- ggplot(bar.df, aes(x = type, y = error,fill=type))+
  geom_bar(stat = "identity",width = 0.3)+
  ylim(0,0.05)+
  geom_text(aes(label=error), vjust=-0.8, size=3.5)+
  theme_minimal()+
  labs(x = "",y="RMS Error",size = 15)+
  theme(
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size=13),
    legend.text = element_text(size = 10))
p


# Neural Network with single layer with 5 nodes
nn.1 <- neuralnet(data = train.df, Price ~., hidden=c(5))
plot(nn.1, rep = "best")#visualize nn.1

# Neural Network with two layers, 5 nodes in each layer
nn.2 <- neuralnet(data = train.df, Price ~., hidden = c(5,5)) 
plot(nn.2, rep = "best")#visualize nn.2

#calculate RMS error for both models
nn1.tr.error <- rmse(train.df$Price, compute(nn.1,train.df)$net.result)#train error for nn.1
nn1.val.error <- rmse(valid.df$Price, compute(nn.1, valid.df)$net.result)#validation error for nn.1

nn2.tr.error <- rmse(train.df$Price, compute(nn.2, train.df)$net.result)#train error for nn.2
nn2.val.error <- rmse(valid.df$Price, compute(nn.2, valid.df)$net.result)#validation error on nn.2

#make a dataframe to visualize
columns <- c("model","a.nn","nn1","nn2")
about <- c("hidden layer","number of nodes","train error","validation error")
prior.nn <- c(1,2,train.error, valid.error)
first <- c(1,5,nn1.tr.error,nn1.val.error)
second <- c(2,5,nn2.tr.error,nn2.val.error)

viz <- data.frame(about,prior.nn,first,second)
colnames(viz)<-columns
viz#visualize


#visualize line plot
line.viz <-data.frame(train.err = c(train.error, nn1.tr.error, nn2.tr.error),
                      valid.err = c(valid.error, nn1.val.error, nn2.val.error),
                      model = c("a.nn","nn1","nn2"))
line.viz
line.viz$train.err
err<-c(line.viz$train.err, line.viz$valid.err)
d <-data.frame(dataset = c(rep("train",3),rep("valid",3)),
               error = err,
               model = rep(c("a.nn","nn1","nn2"),2))


#comparison of three Neural Network Models
ggplot(data = d, mapping = aes(y = err, x = model ,group = dataset, colour=dataset)) +
  geom_line(size=1.2) +
  geom_point(size=5) +
  geom_text(aes(label=round(err,4)), vjust=-0.8, size=5)+
  ylim(0.03,0.045)+
  labs(x = "Model",y="RMS Error",size = 15)+
  theme(
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size=13),
    legend.text = element_text(size = 10))

#i) 레포트에 작성
#ii) 레포트에 작성

###iii)
#추가적 실험 진행
nn.3 <- neuralnet(data = train.df, Price ~., hidden = c(5,5,5)) 
nn3.tr.error <- rmse(train.df$Price, compute(nn.3,train.df)$net.result)#train error for nn.4
nn3.val.error <- rmse(valid.df$Price, compute(nn.3, valid.df)$net.result)#validation error for nn.4

nn.4 <-neuralnet(data = train.df, Price ~., hidden = c(10,10,10))
nn4.tr.error <- rmse(train.df$Price, compute(nn.4,train.df)$net.result)#train error for nn.4
nn4.val.error <- rmse(valid.df$Price, compute(nn.4, valid.df)$net.result)#validation error for nn.4
nn4.val.error

#comparison of all Neural Network Models
ggplot(data = d, mapping = aes(y = err, x = model ,group = dataset, colour=dataset)) +
  geom_line(size=1.2) +
  geom_point(size=5) +
  geom_text(aes(label=round(err,4)), vjust=-0.8, size=5)+
  geom_point(aes(x='nn3', y=nn3.tr.error), colour="blue", shape = 't', size = 5,stroke = 3)+
  geom_point(aes(x='nn3', y=nn3.val.error), colour="blue", shape='v',size = 5,stroke = 3)+
  geom_point(aes(x='nn4', y=nn4.tr.error), colour="red", shape='t',size = 5,stroke = 3)+
  geom_point(aes(x='nn4', y=nn4.val.error), colour="red", shape='v', size = 5,stroke = 3)+
  ylim(0.03,0.048)+
  labs(x = "Model",y="RMS Error",size = 15)+
  theme(
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size=13),
    legend.text = element_text(size = 10))

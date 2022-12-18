
################## 
#      7.2       #
#   2018312824   #
#    류채은      #
##################

###### a.
rm(list=ls())#reset environment

#read data
bank.df <- read_csv("UniversalBank.csv")
colnames(bank.df)
bank.df <- bank.df[,-c(1,5)]#remove ID and ZIp Code variable

#Find Categorical Variables
#install.packages("fastDummies")
library(fastDummies)
summary(bank.df)
table(bank.df$Family)
table(bank.df$Education)

#Categorical Variables with more than two categories in bank.df
#: Family, Education

#one-hot encoding
real.bank.df <- dummy_cols(bank.df, select_columns = c("Family","Education"))
real.bank.df <- real.bank.df[,-c(4,6)]#get rid of original Family & Education variables
head(real.bank.df)



#Partition data: 60% training dataset & 40% validation dataset
set.seed(38)
train.index <- sample(row.names(real.bank.df), dim(real.bank.df)[1]*0.6)
valid.index <- setdiff(row.names(real.bank.df),train.index)
train.df <- real.bank.df[train.index,]
valid.df <- real.bank.df[valid.index,]


#Data Normalization on continuous NUMERIC data only
summary(real.bank.df)#Numeric variables: Age, Experience,Income CCAvg, Mortgage

train.norm <- train.df
valid.norm <- valid.df
whole.norm <- real.bank.df

library(caret)
numeric.names <- c("Age","Experience","Income","CCAvg","Mortgage")
norm.values <- preProcess(train.df[,c("Age","Experience","Income","CCAvg","Mortgage")],method = c("center", "scale"))

train.norm[,numeric.names]<-predict(norm.values, train.df[,numeric.names])
valid.norm[,numeric.names]<-predict(norm.values, valid.df[,numeric.names])
whole.norm[,numeric.names]<-predict(norm.values, real.bank.df[,numeric.names])

library(dplyr)

#예측할 값 (find)
# :이전 캠페인에서 고객에게 제안된 개인 대출을 받아들인 고객들 480명
find <- whole.norm %>%
  filter(`Personal Loan`==1)
colnames(train.norm)

#Training KNN classifier when k = 1
library(FNN)#library for knn
predictors <- c(1:5,7:17)
cl = train.norm[,6]
dim(cl)
dim(train.norm)
dim(find)

#train a knn classifier with threshold=0.5, (success:1, fail:0)
nn <- knn(train = train.norm[,predictors],
          test = find[,predictors],
          cl = train.norm$`Personal Loan`,k=1)


table.nn <- table(nn)#result of classification
bar <- barplot(table.nn,ylim = c(0,450))
text(x = bar, y = table.nn+15, labels = as.character(table.nn))
title(xlab = "Fail:0 Suceess: 1",
      ylab = "number of Customers")

###### b.

num = 16#maximum k values for optimization

#build dataframe to store values calculated on various evaluation metrics 
accuracy.df <- data.frame(k = seq(1, num, 1), 
                          accuracy = rep(0, num), 
                          sensitivity = rep(0,num),
                          specificity=rep(0,num),
                          precision=rep(0,num), 
                          F1=rep(0,num) )
for(i in 1:num) {
  #predict with Knn classifier for different K values.
  knn.pred <- knn(train.norm[,predictors], valid.norm[,predictors], 
                  cl = as.factor(train.norm$`Personal Loan`), k = i)
  ConMat <- confusionMatrix(knn.pred, as.factor(valid.norm$`Personal Loan`), positive="1")
  accuracy.df[i, 2] <- ConMat$overall[1] #Accuracy
  accuracy.df[i, 3] <- ConMat$byClass[c("Sensitivity")]
  accuracy.df[i, 4] <- ConMat$byClass[c("Specificity")]
  accuracy.df[i, 5] <- ConMat$byClass[c("Precision")]
  accuracy.df[i, 6] <- ConMat$byClass[c("F1")]
}

accuracy.df#show accuracy

###### c.

#최적의 k값(3)을 사용하여 예측
predicted.knn <- knn(train.norm[,predictors], valid.norm[,predictors], 
                     cl = as.factor(train.norm$`Personal Loan`), k = 3)


#정오 행렬 표 시각화
#install.packages("ConfusionTableR")
confusionMatrix(predicted.knn, as.factor(valid.norm$`Personal Loan`))
library(ConfusionTableR)

binary_visualiseR(train_labels = predicted.knn,#predicted values
                  truth_labels = as.factor(valid.norm$`Personal Loan`),#ground truth values
                  class_label1 = "FAIL(0)",#label for negative
                  class_label2 = "SUCCESS(1)",#label for positive
                  quadrant_col1 = "#28ACB4", 
                  quadrant_col2 = "#4397D2", 
                  custom_title = "Personal Loan Confusion Matrix",#title
                  text_col= "black",
                  cm_stat_size = 1.2
)

###### d.

# 최적의 K = 3를 사용하여 고객들(find)을 분류
best.knn <- knn(train = train.norm[,predictors],
                test = find[,predictors],
                cl = train.norm$`Personal Loan`,k=3)
#display the classification result in barplot.
bar2<-barplot(table(best.knn),ylim = c(0,400))
text(x = bar2, y = table(best.knn)+15, labels = as.character(table(best.knn)))
title(xlab = "Fail:0 Suceess: 1", ylab = "number of Customers")


###### e.

#Partition dataset: 50%: training 30%: validation 20%: testing
set.seed(38)#set seed
#whole.norm: normalized entire data

# Train dataset 50%
train.index <- sample(rownames(whole.norm),dim(whole.norm)[1]*0.5)#indices for training
train.data <- whole.norm[train.index,]

# Validation dataset 30%
valid.index <- sample(setdiff(rownames(whole.norm),train.index),dim(whole.norm)[1]*0.3)#indices for validation
valid.data <- whole.norm[valid.index,]

# Test dataset 20%
test.index <- setdiff(rownames(whole.norm),union(train.index, valid.index))
test.data <- whole.norm[test.index,]

knn.pred.train <- knn(train.data[,predictors], valid.data[,predictors], cl = as.factor(train.data$`Personal Loan`), k = 3)
dim(valid.data)
dim(test.data)
#visualize confusion matrix for training and validation dataset
binary_visualiseR(train_labels = knn.pred.train,#predicted values
                  truth_labels = as.factor(valid.data$`Personal Loan`),#ground truth values
                  class_label1 = "FAIL(0)",#label for negative
                  class_label2 = "SUCCESS(1)",#label for positive
                  quadrant_col1 = "#28ACB4", 
                  quadrant_col2 = "#4397D2", 
                  custom_title = "Confusion Matrix (Train & Validation)",#title
                  text_col= "black",
                  cm_stat_size = 1.2
)

#train and validation data altogether
trainvalid.data <- rbind(train.data,valid.data)

# Classify on a test dataset
knn.pred.test <- knn(trainvalid.data[,predictors], test.data[,predictors], cl = as.factor(trainvalid.data$`Personal Loan`), k = 3)

CrossTable(x=test.data$`Personal Loan`,y=knn.pred.test, prop.chisq = FALSE)

#visualize confusion matrix for training and validation dataset
binary_visualiseR(train_labels = knn.pred.test,#predicted values
                  truth_labels = as.factor(test.data$`Personal Loan`),#ground truth values
                  class_label1 = "FAIL(0)",#label for negative
                  class_label2 = "SUCCESS(1)",#label for positive
                  quadrant_col1 = "#28ACB4", 
                  quadrant_col2 = "#4397D2", 
                  custom_title = "Confusion Matrix (Test)",#title
                  text_col= "black",
                  cm_stat_size = 1.2
)

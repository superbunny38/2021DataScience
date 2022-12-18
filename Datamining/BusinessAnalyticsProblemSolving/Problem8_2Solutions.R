
################## 
#      8.2       #
#   2018312824   #
#    류채은      #
##################

rm(list=ls())#reset environment
accidents.df <- read.csv("accidentsFull.csv")#read data
#to see if the size match the size(42,183) stated in problem
dim(accidents.df)
head(accidents.df)
summary(accidents.df)

#가변수 생성(MAX_SEV_IR이 1이나 2면 Yes, 그렇지 않으면 0)
accidents.df$INJURY <- ifelse(accidents.df$MAX_SEV_IR>0,"Yes","No")
accidents.df$INJURY <- as.factor(accidents.df$INJURY)

###### a.
#make a pie but use the numeric output instead of pie chart.
accidents_var <- as.factor(accidents.df$INJURY)
accidents <- data.frame(acci = accidents_var)
PieChart(acci, hole = 0,
         values = "%", data = accidents,
         fill = c("lightblue","pink"),
         main = "",
         values_color = "black")

###### b
options(digits=3)
colnames(accidents.df)#get column names of accidents data
use.df <- head(accidents.df[,c(19,16,25)],12)#only use variables WEATHER_R, TRAF_CON_R, INJURY

#convert variables to categorical type
for (i in c(1:3)){
  use.df[,i]<-as.factor(use.df[,i])
}

original.use.df <- use.df
use.df#데이터 확인

###i)
#make a pivot table for all three variables(WEATHER_R, TRAF_CON_R, INJURY)
table(use.df$WEATHER_R, use.df$TRAF_CON_R, use.df$INJURY,dnn = c("WEATHER_R","TRAF_CON_R", "INJURY"))

###ii) 수기로 계산 후 레포트에 첨부

###iii)
accidents<-c(0.667,0.167,0,0,0.667,0.167,0.167,0.667,0.167,0.167,0.167,0)#수기로 계산한 확률
use.df$prob.injury <- accidents
#classify data samples with threshold value 0.5
use.df$estimated <- ifelse(use.df$prob.injury>0.5,"Yes","No")
use.df#display output

###iv) 수기로 레포트에 첨부

###v)
##load packages and run the naive Bayes classifier
library(e1071)
library(klaR)
library(caret)
colnames(original.use.df)

#one-hot encoding
dummy.df <- dummy_cols(original.use.df, select_columns = c("WEATHER_R","TRAF_CON_R"))
colnames(dummy.df)
dummy.df <- dummy.df[,-c(1,2)]#get rid of "WEATHER_R","TRAF_CON_R" variables
head(dummy.df)
for (i in c(1:dim(dummy.df)[2])){
  dummy.df[,i]<-as.factor(dummy.df[,i])
}

input=dummy.df[,-1]
target=dummy.df$INJURY

#11-fold cross validation (k = 11)
model <- train(input,target,'nb', trControl = trainControl(method = 'cv',number=11))

model.pred<-predict(model$finalModel,input)#predict with naive bayes model

#make a dataframe to visualize classification result of naive bayes classifier
output.df <- data.frame(actual = dummy.df$INJURY,
                        nb.class = model.pred$class,
                        nb.prob = model.pred$posterior)

output.df#display output of naive bayes classifier

###Create barplot to compare accuracy of two classification results(naive bayes classifier and manual calculation)

accuracy.nb <- ifelse(output.df$actual == output.df$nb.class,"Correct","Incorrect")
nb.tbl <- table(accuracy.nb)

accuracy.manual <- ifelse(output.df$actual == use.df$estimated,"Correct","Incorrect")
manual.tbl <- table(accuracy.manual)

#display table for both classification results
nb.tbl#naive bayes classification result
manual.tbl#manual classification result


###### c.
#Partition data(training: 60%, validation: 40%)
set.seed(22)
#categorize variables
for (i in c(1:dim(accidents.df)[2])){
  accidents.df[,i] <- as.factor(accidents.df[,i])
}
train.index <- sample(c(1:dim(accidents.df)[1]), dim(accidents.df)[1]*0.6)  
train.df <- accidents.df[train.index,]
valid.df <- accidents.df[-train.index,]

#i) 레포트에 작성
#ii)
#variables to use
colnames(train.df)[1]<-"ï..HOUR_I_R"#깨진 글자 복구
use.var <- c("INJURY","ï..HOUR_I_R","ALIGN_I","WRK_ZONE","WKDY_I_R","INT_HWY","LGTCON_I_R","PROFIL_I_R","SPD_LIM","SUR_COND","TRAF_CON_R","TRAF_WAY","WEATHER_R")

#train a naive bayes classifier
#Simple NB
Nb.Full <- naiveBayes(INJURY ~ ., data = train.df[,use.var])
predicted.Full <-predict(Nb.Full, train.df[, use.var])

library(yardstick)

#create confusion matrix
cm <- confusionMatrix( predicted.Full,train.df$INJURY, positive = "Yes")
cm$table
cm
#visualize confusion matrix
library(ConfusionTableR)
dev.off()
binary_visualiseR(train_labels = predicted.Full,#predicted values
                  truth_labels = train.df$INJURY,#ground truth values
                  class_label1 = "No",#label for negative
                  class_label2 = "Yes",#label for positive
                  quadrant_col1 = "lightblue", 
                  quadrant_col2 = "pink", 
                  custom_title = "SIMPLE NB: Confusion Matrix",#title
                  text_col= "black",
                  cm_stat_size = 1.2
)
use.var.x <- c("ï..HOUR_I_R","ALIGN_I","WRK_ZONE","WKDY_I_R","INT_HWY","LGTCON_I_R","PROFIL_I_R","SPD_LIM","SUR_COND","TRAF_CON_R","TRAF_WAY","WEATHER_R")

#Build Complex NB Model
x<- train.df[,use.var.x]
y<-train.df$INJURY
nb1 <- train(x,y,'nb', trControl = trainControl(method = 'cv',number=5))
nb2 <- train(x,y,'nb', trControl = trainControl(method = 'repeatedcv',number=10,repeats = 5))
nb3 <- train(x,y,'nb', trControl = trainControl(method = 'boot',number = 3))

#predict with Complex NB Model
nb1.pred <- predict(nb1$finalModel,x)
nb2.pred <- predict(nb2$finalModel,x)
nb3.pred <- predict(nb3$finalModel,x)

#최빈값 함수
## 최빈값 구하기
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#분류값 한 곳에 저장하기
preds <- cbind(nb1.pred$class,nb2.pred$class,nb3.pred$class)

final.preds <- preds[1]

for (i in c(1:dim(preds)[1])){
  final.preds[i]<-Mode(preds[i,])
}

map.final <- ifelse(final.preds == 1,"No","Yes")
map.final <- as.factor(map.final)
confusionMatrix(map.final,train.df$INJURY,positive = "Yes")

#iii)
colnames(valid.df)[1]<-"ï..HOUR_I_R"#깨진 글자 복구
valid.pred <- predict(Nb.Full, valid.df[, use.var])

#Confusion matrix for validation dataset
cf <- confusionMatrix(valid.pred, valid.df$INJURY, positive = "Yes")

#visualize confusion matrix
binary_visualiseR(train_labels = valid.pred,#predicted values
                  truth_labels = valid.df$INJURY,#ground truth values
                  class_label1 = "No",#label for negative
                  class_label2 = "Yes",#label for positive
                  quadrant_col1 = "lightblue", 
                  quadrant_col2 = "pink", 
                  custom_title = "Validation: Confusion Matrix",#title
                  text_col= "black",
                  cm_stat_size = 1.2
)
error.rate <- round(100 * (1 - cf$overall[[1]]))
error.rate

#iv)

#Naive Rule
naive_cf <- confusionMatrix(
  data = as.factor(rep("Yes", dim(valid.df)[[1]])),
  reference = valid.df$INJURY
)

naive_cf$overall[[1]]

bar.df <-data.frame(classification.method = c("NaiveRule","NaiveBayes"),
                    accuracy = c(naive_cf$overall[[1]],cf$overall[[1]]))
ggplot(bar.df,
       aes(x=classification.method,y=accuracy,color=classification.method))+
  geom_bar(stat="identity",fill = "white")+
  geom_text(aes(label = round(accuracy,2)),vjust=1.6,size=10)#소수점 두번째 자리까지만 출력

#v)
freq.tbl <- data.frame(table(valid.df$INJURY, valid.df$SPD_LIM,dnn = c("INJURY","SPD_LIM")))
freq.tbl$p <- freq.tbl$Freq/sum(freq.tbl$Freq)
head(freq.tbl)
sum(freq.tbl$Freq)



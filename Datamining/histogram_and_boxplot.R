## histogram of MEDV
hist(housing.df$MEDV, xlab = "MEDV")

#alternative plot with ggplot
library(ggplot2)

ggplot(housing.df)+geom_histogram(aes(x=MEDV),binwidth = 5)#range=5
housing.df$CRIM
ggplot(housing.df)+geom_histogram(aes(x=CRIM),binwidth = 3)#range=3


#box plot of MEDV
boxplot(housing.df$MEDV ~ housing.df$CHAS, xlab = "CHAS", ylab= "MEDV")
#alternative plot with ggplot
ggplot(housing.df)+geom_boxplot(aes(x=as.factor(CHAS),y=MEDV))+xlab("CHAS")+ylab("MEDV")

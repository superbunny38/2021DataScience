## line chart for the Amtrak data
Amtrak.df <- read.csv("Amtrak.csv")

#use time series analysis
library(forecast)
ridership.ts <- ts(Amtrak.df$Ridership, start = c(1991,1),end=c(2004,3),freq=12)
plot(ridership.ts, xlab="Year",ylab="Ridership (in 000s)", ylim = c(1300,2300))


#Boston housing data
housing.df <- read.csv("boston.csv")

# scatter plot with axes names
plot(housing.df$MEDV ~ housing.df$LSTAT, xlab="MEDV",ylab="LSTAT")


#alternative plot with ggplot
library(ggplot2)
ggplot(housing.df)+geom_point(aes(x=LSTAT, y = MEDV),colour="navy", alpha=0.7)

#bar chart of CHAS vs. mean MEDV
#compute mean MEDV per CHAS = (0,1)
housing.df$CHAS#binary
head(housing.df$MEDV)
mean(housing.df$MEDV)

data.for.plot <- aggregate(housing.df$MEDV, by = list(housing.df$CHAS), FUN=mean)
head(data.for.plot)                           
names(data.for.plot) <- c("CHAS","meanMEDV")
head(data.for.plot)
barplot(data.for.plot$meanMEDV, names.arg = data.for.plot$CHAS, xlab="CHAS",ylab="Avg. MEDV")

#alternative plot with ggplot
ggplot(data.for.plot)+geom_bar(aes(x=CHAS,y=meanMEDV),stat="identity")


#barchart of CHAS vs. % CAT.MEDV
head(housing.df$CAT..MEDV)#binary
summary(housing.df$CAT..MEDV)
data.for.plot <- aggregate(housing.df$CAT..MEDV, by = list(housing.df$CHAS),FUN=mean)
names(data.for.plot)<-c("CHAS","MeanCATMEDV")
head(data.for.plot)
barplot(data.for.plot$MeanCATMEDV*100, names.arg = data.for.plot$CHAS, xlab="CHAS",ylab="% of CAT.MEDV")

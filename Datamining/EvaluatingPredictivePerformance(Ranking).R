#Creating a lift chart: two options
df <- read.csv("liftExample.csv")

#first option with "caret" library
library(caret)
lift.example <- lift(relevel(as.factor(actual), ref = "1")~prob, data=df)
xyplot(lift.example, plot="gain")

#second option with 'gains' library
library(gains)
gain <- gains(df$actual, df$prob, groups = dim(df)[1])
plot(c(0,gain$cume.pct.of.total*sum(df$actual))~c(0,gain$cume.obs),
     xlab = "# cases", ylab = "Cumulative", type = "l")
lines(c(0,sum(df$actual))~c(0,dim(df)[1]),col="gray",lty=2)

#decile lift chart
#use gains() to compute deciles.

gain <- gains(df$actual, df$prob,)
barplot(gain$mean.resp/mean(df$actual), names.arg = gain$depth, xlab = "Percentile",
        ylab = "Mean Response", main = "Decile-wise lift chart")

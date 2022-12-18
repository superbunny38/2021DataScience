height <- c(166,186,159,174,172,148,159,171,163,165)

mean.height <- mean(height)
mean.adjusted <- height - mean.height
sd.height <- sd(height)
sd.height

standardized <- mean.adjusted/sd.height
summary(standardized)#max: 3*sd, min: -3*sd

mean(standardized)#0에 근접
var(standardized)#1
sd(standardized)#1

prep.height <- cbind(height, mean.adjusted, standardized)
head(prep.height)


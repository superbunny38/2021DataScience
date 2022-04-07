
### Summary Statistics for the Boston Housing Data
boston.housing.df <- read.csv("boston.csv", header = TRUE)
head(boston.housing.df, 9)
summary(boston.housing.df)

#compute mean, std, min, max, median, length, and missing value of CRIM
mean(boston.housing.df$CRIM)
sd(boston.housing.df$CRIM)#std
max(boston.housing.df$CRIM)
median(boston.housing.df$CRIM)
length(boston.housing.df$CRIM)

#find the number of missing values of variable CRIM
sum(is.na(boston.housing.df$CRIM))

#compute mean, standard dev., min, max, median, length, and missing value for all variables.
data.frame(mean = sapply(boston.housing.df, mean), 
             sd = sapply(boston.housing.df, sd),
             min = sapply(boston.housing.df, min),
             max = sapply(boston.housing.df, max),
             median = sapply(boston.housing.df, median),
             length = sapply(boston.housing.df, length),
           miss.val = sapply(boston.housing.df, function(x) sum(length(which(is.na(x))))))

#correlation table for data
round(cor(boston.housing.df),2)

#pivot table
table(boston.housing.df$CHAS)

#create bins of size 1
head(boston.housing.df$RM)
summary(boston.housing.df$RM)
.bincode(boston.housing.df$RM,c(1:9))
summary(.bincode(boston.housing.df$RM,c(1:9)))

#create bins of size 1
boston.housing.df$RM.bin <- .bincode(boston.housing.df$RM,c(1:9))#방 1개~방9개까지 categorize

#compute the average of MEDV by (binned) RM and CHAS
#in aggregate() use the by = to define the list of aggregating variables,
#and FUN = as an aggregating function
aggregate(boston.housing.df$MEDV, by = list(RM = boston.housing.df$RM.bin,
                                            CHAS = boston.housing.df$CHAS), FUN = mean)

head(boston.housing.df$RM)
head(boston.housing.df$RM.bin)
summary(boston.housing.df$RM.bin)


#***************강의자료에 안 나옴**************#
library(reshape)
#use melt() to stack a set of columns into a single column of data
#stack MEDV values for each combination of (binned) RM and CHAS
mlt <- melt(boston.housing.df, id = c("RM.bin","CHAS"), measure = c("MEDV"))
head(mlt,5)

#use cast() to reshape data and generate pivot table
cast(mlt, RM.bin ~ CHAS, subset = variable == "MEDV",
     margins = c("grand_row", "grand_col"),mean)
#***********************************************#


#Multicollinearity(다중공산성): the presence of two or more predictors sharing the same linear relationship with the output variable.

#PCA on the two variables Calories and Rating

cereals.df <- read.csv("Cereals.csv")
#compute PCs on two dimensions
original <- data.frame(cereals.df$calories, cereals.df$rating)
cov(original)
summary(original)
head(original)
pcs <- prcomp(original)
summary(pcs)

pcs$rotation#rotation matrix, which gives the weights that are used to project the original points onto the two new directions.

scores <- pcs$x
head(scores,5)

head(original)

weights <- pcs$rotation
weights <- data.frame(weights)
weights


#PCA output using all 13 numerical variables in the breakfast cereals dataset.
pcs <- prcomp(na.omit(cereals.df[,-c(1:3)]))
summary(pcs)

pcs$rotation

head(pcs$x)


#PCA output using all normalized 13 numerical variables in the breakfast cereals dataset.

pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]),scale. = T)
summary(pcs.cor)
pcs.cor$rot[,1:5]

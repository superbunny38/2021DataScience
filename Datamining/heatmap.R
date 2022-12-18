## simple heatmap of correlations (without values)

heatmap(cor(housing.df),Rowv = NA, Colv = NA)

## heatmap with values
#install.packages("gplots")
library(gplots)
heatmap.2(cor(housing.df), Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          cellnote = round(cor(housing.df),2),
          notecol = "black",
          key = FALSE, trace = "none", margins = c(10,10))

#alternative plot with ggplot
library(ggplot2)
library(reshape)#to generate input for the plot

cor.mat <- round(cor(housing.df),2)#rounded correlation matrix
melted.cor.mat <- melt(cor.mat)
ggplot(melted.cor.mat, aes(x = X1, y = X2, fill = value))+
  geom_tile()+
  geom_text(aes(x=X1, y = X2, label = value))

dim(housing.df)


#replace the dataFrame with your data
sum(is.na(housing.df))
# is.na() returns a Boolean (TRUE/FALSE) output indicating the location of missing values
# multiplying the Boolean value by 1 converts the output into binary (0/1).
dataFrame <- housing.df
rows.to.missing <- sample(dim(housing.df)[1], 21)
dataFrame[rows.to.missing,c(2,10)] <- NA
sum(is.na(housing.df))
sum(is.na(dataFrame))
heatmap(1*is.na(dataFrame), Rowv = NA, Colv = NA)


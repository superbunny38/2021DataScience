# 3.1 Shipments of Household Appliances : Line Graphs
p1.df <- read.csv("ApplianceShipments.csv")
head(p1.df)
tail(p1.df)
# a. Create a well-formatted time plot for the data using R
ship.ts <- ts(p1.df$Shipments, start = 1, end = 20, freq = 1)
ship.ts
plot(ship.ts, xlab = "Quarter", ylab = "shipments")

# b. Does there appear to be a quarterly pattern?
# For a closer view of the patterns, zoom in to the range of 3500-5000 on the y-axis
plot(ship.ts, xlab = "Quarter", ylab = "shipments", ylim = c(3500,5000))
# Yes there appear to be a quarterly pattern.

# c. Using R, create one chart with four separate lines, one line for each of Q1, Q2, Q3, and Q4.
# In R, this can be achieved by generating a data.frame for each quarter Q1, Q2, Q3, Q4, and then plotting them as separate series on the line graph.
# Zoom in to the range of 3500-5000 on the y-axis. Does there appear to be a difference between quarters?
head(p1.df)

all.nums <- c(1:20)
Q1.rows <- all.nums[all.nums%%4 == 1]
Q2.rows <- all.nums[all.nums%%4 == 2]
Q3.rows <- all.nums[all.nums%%4 == 3]
Q4.rows <- all.nums[all.nums%%4 == 0]

Q1 <- p1.df[Q1.rows,]
Q2 <- p1.df[Q2.rows,]
Q3 <- p1.df[Q3.rows,]
Q4 <- p1.df[Q4.rows,]

separated.df <- cbind(Q1$Shipments,Q2$Shipments,Q3$Shipments, Q4$Shipments)
separated.df <- as.data.frame(separated.df)
names(separated.df) <- c("Q1","Q2","Q3","Q4")
separated.df
separated.ts <- ts(separated.df)
plot(separated.ts, ylim=c(3500,5000))

# d. Using R, create a line graph of the series at a yearly aggregated level (i.e., the total shipments in each year)

head(p1.df)
tail(p1.df)
#c(c(1985:1989),c(1985:1989),c(1985:1989),c(1985:1989))
p1.df$Year <- c(rep(1985,4), rep(1986,4), rep(1987,4), rep(1988,4), rep(1989,4))
head(p1.df)
yearly.df <- aggregate(p1.df$Shipments, by = list(p1.df$Year), FUN = sum)
y.series <- ts(yearly.df$x, start = 1985, end = 1989)
plot(y.series, xlab = "year")


# 3.2 Sales of Riding Mowers: Scatter Plots
# a. Using R, create a scatter plot of Lot Size vs. Income, color-coded by the outcome variable owner/nonowner.
# Make sure to obtain a well-formatted plot (create legible lables and a legend, etc. )
p2.df <- read.csv("RidingMowers.csv")
head(p2.df)
# y ~ x
plot(p2.df$Income ~ p2.df$Lot_Size, col = factor(p2.df$Ownership),pch=1, xlab = "Lot size", ylab = "Income")
# x, y
plot(p2.df$Lot_Size, p2.df$Income, col = factor(p2.df$Ownership), xlab = "Lot size", ylab = "Income")

# 3.3 Laptop Sales at a London Computer Chain: Bar Charts and Boxplots.
# a. Create a bar chart, showing the average retail price by store.
# Which store has the highest average? Which has the lowest?
p3.df <- read.csv("LaptopSalesJanuary2008.csv")
head(p3.df)
summary(p3.df$Store.Postcode)
str(p3.df$Store.Postcode)
p3.a.df <- cbind(p3.df$Store.Postcode, p3.df$Retail.Price)
head(p3.a.df)
p3.a.df <- as.data.frame(p3.a.df)
names(p3.a.df) <- c("Store", "RetailPrice")
head(p3.a.df)
summary(p3.a.df$RetailPrice)
p3.a.df$RetailPrice <- as.numeric(p3.a.df$RetailPrice)
sum(is.na(p3.a.df$RetailPrice))
data.for.plot <- aggregate(p3.a.df$RetailPrice, by = list(p3.a.df$Store), FUN = mean)
head(data.for.plot)

barplot(data.for.plot$x, names.arg = data.for.plot$Group.1, xlab = "Store", ylab="RetailPrice")

summary(data.for.plot)

head(data.for.plot[order(data.for.plot$x),])#store w/ minimum retail price = W4
tail(data.for.plot[order(data.for.plot$x),])#store w/ maximum retail price = N17

# b. To better compare retail prices across stores, create side-by-side boxplots of retail price by store.
# Now compare the prices in the two stores from (a).
# Does there seem to be a difference between their price distribution?
library(ggplot2)
ggplot(data.for.plot) + geom_boxplot(aes(x = as.factor(Group.1), y = x)) + xlab("store") + ylab("retail price")

# 3.4 Laptop Sales at a London Computer Chain: Interactive Visualization
p4.df <- read.csv("LaptopSales.csv")
head(p3.df)
# a. Price Questions:
# i. At what price are the laptops actually selling?
summary(p3.df$Retail.Price)
# Min: 300 ~ Max: 665.0

# ii. Does price change with time? ( Hine: Make sure that the data column is recognized as such. )
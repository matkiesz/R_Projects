#load libraries

library(gridExtra)
library(corrplot)
library(ggplot2)

#load dataset
rectask <- read.csv("train.csv", header = TRUE, row.names = 1, sep = ",", dec = ".")
test <- read.csv("test.csv", header = TRUE, row.names = 1, sep = ",", dec = ".")

#basic exploration

dim(rectask)
summary(rectask)
#contain missing values?
sum(is.na(rectask))
#3750 rows, 21 variables, no missing values

graphlist <- list()
for (i in names(rectask)) {
  graphlist[[i]] <- ggplot(rectask, aes_string(x = i)) + geom_histogram(fill = "SteelBlue")+ labs(y = "")
}

do.call("grid.arrange" , c(graphlist,ncol=4))


corrplot(cor(rectask))
corrplot(cor(rectask), method = "number")
corrplot(cor(rectask), method = "number", type = "upper")
corrplot(cor(rectask), method = "number", type = "upper", number.cex = 0.55)

ggplot(rectask, aes(x = feat18, y = feat6)) + geom_point() + geom_smooth()

rectask2 <- subset(rectask, select = -c(feat7,feat12, feat17, feat13,feat20))


library(Biobase)
library(breastCancerUPP)
data(upp)
y <- factor(pData(upp)$er, labels=c("ER-", "ER+"))
x <- t(exprs(upp))[!is.na(y),]
y <- y[!is.na(y)]


library(Biobase)
library(breastCancerUPP)
data(upp)
x <- t(exprs(upp))
y <- factor(pData(upp)$er, labels=c("ER-", "ER+"))

Sys.sleep(3)


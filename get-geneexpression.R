.libPaths("../vendor")

library("Biobase")
data("upp", package = "breastCancerUPP")

y <- factor(pData(upp)$er, labels = c("ER-", "ER+"))
x <- as.matrix(t(exprs(upp))[!is.na(y), ])
y <- y[!is.na(y)]

if (exists("remove.values") && remove.values) {
    x[sample(length(x), 100)] <- NA
}


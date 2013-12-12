
# Section 2.1 -- Standard use

data(iris)
cv <- resample.crossval(iris$Species, nrep=5, nfold=5)
my.analysis <- batch.predict(x = iris[1:4], y = iris$Species,
    test.subset = cv, pre.trans = pre.impute.median,
    model = "rf", error.fun = error.rate, .verbose = TRUE)


# Section 2.2 -- Custom use



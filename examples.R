
# Install and load required packages
required.pkg <- c("emil", "randomForest", "survival")
installed.pkg <- sapply(required.pkg, require, character.only = TRUE)
if(any(!installed.pkg)){
    install.packages(required.pkg[!installed.pkg])
}


#===============================================================================
#   Section 2.1: Standard usage
#-------------------------------------------------------------------------------

# Create a dataset
x.train <- sweep(matrix(rnorm(60 * 10), 60), 1, rep(c(0, .8), each = 30), "+")
y.train <- gl(2, 30, labels = c("class A", "class B"))

x.test <- sweep(matrix(rnorm(60 * 10), 60), 1, rep(c(0, .8), each = 30), "+")
y.test <- gl(2, 30, labels = c("class A", "class B"))

# Create and tuned a modeling procedure
proc <- modeling.procedure(method = "glmnet", param = list(alpha = 0:4 / 4))
tuned.proc <- tune(proc, x.train, y.train)
model <- fit(tuned.proc, x.train, y.train)

# Predict the response of the test set
pred <- predict(proc, model, x.test)

# Calculate the error
error.rate(y.test, pred)

# Do all of the above in a resampled version
x <- rbind(x.train, x.test)
y <- factor(c(y.train, y.test))
ho <- resample("holdout", y, frac = .5, nfold = 9)
perf <- evaluate.modeling(proc, x, y, resample = ho)

subtree(perf, 1:9, "error")
mean(subtree(perf, 1:9, "error"))
subframe(perf, TRUE, "pred", "prob", 1, resample = ho)

pdf("image-holdout.pdf", 5 / cm(1), 10 / cm(1))
par(mar = c(3, 3, .5, .5), ps = 8, mgp = c(1.7, .7, 0), tcl = -.3)
image(ho, y)
dev.off()


#===============================================================================
#   Section 2.3: Customization
#-------------------------------------------------------------------------------

modeling.procedure(
    method = "randomForest",
    param = list(mtry = c(20, 100, 400)),
    error.fun = error.rate,
    fit.fun = emil.fit.randomForest,
    predict.fun = emil.predict.randomForest,
    vimp.fun = emil.vimp.randomForest)

batch.model(proc, x, y,
    resample = resample("crossval", y, nfold = 5, nrep = 3),
    pre.process = pre.impute.knn)

tune(proc, x, y,
    pre.process = function(x, y, fold) pre.impute.knn(x, y, fold, k = 5)
)

# Define a (not so) custom method called "myMethod"
emil.fit.myMethod <- emil.fit.glmnet
emil.predict.myMethod <- emil.predict.glmnet

# Debug it using the standard facilities
proc <- modeling.procedure("myMethod")
debug(proc$fit.fun)
perf <- evaluate.modeling(proc, x, y)


#===============================================================================
#   Section 2.4: Model comparison
#-------------------------------------------------------------------------------

proc <- list(rf = modeling.procedure("randomForest"),
             cf = modeling.procedure("cforest"))
perf <- evaluate.modeling(proc, x, y, resample = ho)
subtree(perf, TRUE, c("rf", "cf"), "error")


#===============================================================================
#   Section 3.1: A parallelized simulation
#-------------------------------------------------------------------------------

# Set up the problem
x <- matrix(rnorm(100 * 10000), 100, 10000)
y <- gl(2, 50)
cv <- resample("crossval", y, nfold = 8, nrep = 4)

# Evaluate the sequential solution
proc <- modeling.procedure("randomForest", param = list(ntree = 8000))
system.time(perf.seq <- evaluate.modeling(proc, x, y, resample = cv))

# Evaluate the standard parallel solution
system.time(perf.par1 <- evaluate.modeling(proc, x, y, resample = cv,
                                           .parallel.cores = 16))

# Set up and evaluate the alternative parallel solution
library("parallel")
options(mc.cores = 16)
parProc <- proc
parProc$fit.fun <- function(..., ntree){
    # Calculate how many trees each core needs compute
    nc <- getOption("mc.cores")
    ntree <- table(findInterval(1:ntree - 1, ntree / nc * 1:nc))

    # Fit the cores' forests
    forests <- mclapply(ntree, function(nt) randomForest(..., ntree = nt))

    # Combine the cores' forests into a single forest
    do.call(combine, forests)
}
system.time(perf.par2 <- evaluate.modeling(parProc, x, y, resample = cv))


#===============================================================================
#   Section 3.2: Customized survival analysis modeling
#-------------------------------------------------------------------------------

# Install necessary bioconductor packages
required.pkg <- c("Biobase", "breastCancerUPP")
installed.pkg <- sapply(required.pkg, require, character.only = TRUE)
if(any(!installed.pkg)){
    source("http://bioconductor.org/biocLite.R")
    biocLite(required.pkg[!installed.pkg])
    for(p in required.pkg[!installed.pkg])
        require(p, character.only = TRUE)
}

# Load data
data("upp")
pheno <- pData(upp)
sample.idx <- with(pheno, treatment %in% 2 & !is.na(t.rfs) & !is.na(e.rfs))
x <- t(exprs(upp))[sample.idx,]
y <- with(pheno[sample.idx,],
          outcome(t.rfs, factor(e.rfs, levels = 1, labels = "relapse")))

# Set up method
pre.pca <- function(x, y, fold){
    pca <- prcomp(x[index.fit(fold), , drop = FALSE], scale. = TRUE)
    list(fit = pca$x,
         test = predict(pca, x[index.test(fold),]))
}
proc <- modeling.procedure(
    fit.fun = function(x, y, nfeat){
        list(nfeat = nfeat,
             cox = coxph(as.Surv(y) ~ ., data.frame(x[, 1:nfeat, drop = FALSE])))
    },
    predict.fun = function(object, x)
        list(risk = predict(object$cox,
            data.frame(x[, 1:object$nfeat, drop = FALSE]), type = "risk")),
    param = list(nfeat = c(1, 2, 3, 5, 9, 15, 25))
)


# Run
ho <- resample("holdout", y, frac = .25, nrep = 10)
perf <- evaluate.modeling(proc, x, y, resample = ho, pre.process = pre.pca,
                          .save = list(pred = TRUE, tuning = TRUE))

# Present results
error <- subtree(perf, TRUE, "error")
all.tuning.errors <- subtree(perf, TRUE, "tuning", "error")
mean.tuning.errors <- sapply(all.tuning.errors, apply, 1, mean)

# Plots tuning performance of the folds
matplot(unlist(proc$tuning$param), -mean.tuning.errors, type = "l", lty = 1, las = 1,
        col = "grey", xlab = "Number of PCA components", ylab = "Mean Harrell's C")
lines(unlist(proc$tuning$param), -apply(mean.tuning.errors, 1, mean), lwd = 2)
dev.off()


#===============================================================================
#   Section 3.3: Methylation based subtyping
#-------------------------------------------------------------------------------

# Load or download data
if(file.exists("data/all_methylation.Rdata")){
    load("data/all_methylation.Rdata")
} else {
    source("download_methylation.R")
}
y <- factor(ifelse(all.pheno$subtype %in% c("T-ALL", "t(12;21)", "HeH"),
                   as.character(all.pheno$subtype), "other"))

# Pre-calculate distance matrix for kNN-imputation
if(file.exists("data/all_dist.Rdata")){
    load("data/all_dist.Rdata")
} else {
    all.dist <- dist(all.met)
    save(all.dist, file = "data/all_dist.Rdata")
}

# Set up tracing pre-processing functions
tracing.pre.impute.knn <- function(...){
    sets <- pre.impute.knn(..., k = 10, distmat = all.dist)
    cat("Imputed training set:", tracemem(sets$fit), "\n")
    cat("Imputed test set:",     tracemem(sets$test), "\n")
    return(sets)
}
tracing.pre.pamr <- function(...){
    sets <- pre.pamr(..., pre.process = tracing.pre.impute.knn)
    cat("Re-oriented training set:", tracemem(sets$fit$x), "\n")
    cat("Re-oriented test set:",     tracemem(sets$test), "\n")
    return(sets)
}

# Execute modeling
proc <- modeling.procedure("pamr")
ho <- resample("holdout", y, frac = 1/4, nfold = 1)
cat("Complete data set:", tracemem(all.met), "\n")
pred <- evaluate.modeling(proc, all.met, y, resample = ho,
                          pre.process = tracing.pre.pamr)


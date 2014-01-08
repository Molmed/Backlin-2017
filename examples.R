source.all("~/Documents/R/egna paket/predict/predict/R/")
source.all("~/private/predict/predict/R/")
library(foreach)
library(survival)

#--------------------------------------------------------------[ The real file ]

# Install necessary CRAN packages
required.pkg <- c("survival", "predict")
installed.pkg <- sapply(required.pkg, require, character.only=TRUE)
if(any(!installed.pkg)){
    install.packages(required.pkg[!installed.pkg])
}


#===============================================================================
#   Section 3.1: A parallelized simulation
#-------------------------------------------------------------------------------

set.seed(123)
x <- matrix(rnorm(100*10000), 100, 10000)
y <- gl(2, 50)

# Define an ordinary sequential random forest process
proc <- modelling.procedure("randomForest", param=list(ntree=3000))

# Define a parallelized processes by replacing the fitting function
library(parallel)
options(mc.cores = 3)
parProc <- proc
parProc$fit.fun <- function(..., ntree){
    # Calculate how many trees each worker needs compute
    nc <- getOption("mc.cores")
    ntree <- table(findInterval(1:ntree, ntree/nc * (seq_along(nc)-1)))

    # Fit the workers' forests
    forests <- mclapply(ntree, function(nt) randomForest(..., ntree=nt))

    # Combine the workers' forests into a single forest
    do.call(combine, forests)
}

# Compare computation time
library(randomForest) # Preload to not affect timing
system.time(model <- fit(proc, x, y))
#   user  system elapsed 
# 89.024   0.182  89.152 

system.time(parModel <- fit(parProc, x, y))
#   user  system elapsed 
# 62.836   0.073  34.016 


#===============================================================================
#   Section 3.2: Customized survival analysis modelling
#-------------------------------------------------------------------------------

# Install necessary bioconductor packages
required.pkg <- c("Biobase", "breastCancerUPP")
installed.pkg <- sapply(required.pkg, require, character.only=TRUE)
if(any(!installed.pkg)){
    source("http://bioconductor.org/biocLite.R")
    biocLite(required.pkg[!installed.pkg])
}

# Load data
data(upp)
pheno <- pData(upp)
sample.idx <- with(pheno, treatment %in% 2 & !is.na(t.rfs) & !is.na(e.rfs))
x <- t(exprs(upp))[sample.idx,]
y <- with(pheno[sample.idx,],
          outcome(t.rfs, factor(e.rfs, levels=1, labels="relapse")))

# Setup analysis
pre.pca <- function(x, y, fold){
    pca <- prcomp(x[na.fill(!fold, FALSE),,drop=FALSE], scale.=TRUE)
    list(fit = pca$x,
         test = predict(pca, x[na.fill(fold, FALSE),]))
}
proc <- modelling.procedure(
    fit.fun = function(x, y, nfeat){
        list(nfeat = nfeat,
             cox = coxph(as.Surv(y) ~ ., data.frame(x[, 1:nfeat, drop=FALSE])))
    },
    predict.fun = function(fit, x)
        list(risk = predict(fit$cox,
            data.frame(x[, 1:fit$nfeat, drop=FALSE]), type="risk")),
    param = list(nfeat = c(1, 2, 3, 5, 9, 15, 25)))

ho <- resample.holdout(y, frac=.25, nrep=10)

# Run
set.seed(123)
perf <- evaluate.modelling(proc, x, y, ho, pre.pca, .save=list(pred=TRUE, tuning=TRUE))

# Present results
subtree(perf, T, T, "error")

# Plots tuning performance of the folds
mean.err <- sapply(subtree(perf, T, T, "tuning", "error"), apply, 1, mean)
matplot(unlist(proc$tuning$param), -mean.err, type="l", lty=1,
        xlab="Number of PCA components", ylab="Mean Harrell's C")
dev.off()


#===============================================================================
#   Section 3.3: Methylation based subtyping
#-------------------------------------------------------------------------------

if(file.exists("data/all_methylation.Rdata")){
    load("data/all_methylation.Rdata")
} else {
    source("download_methylation.R")
}

y <- factor(all.pheno$subtype, levels=c("T-ALL", "t(12;21)", "HeH"))
all.met <- all.met[!is.na(y),]
y <- y[!is.na(y)]

proc <- modelling.procedure("pamr")
cv <- resample.crossval(y, 5, 1)

# Pre-calculate the distance matirx, to avoid recalculating it in every fold
if(file.exists("data/all_dist.Rdata")){
    load("data/all_dist.Rdata")
} else {
    all.dist <- dist(all.met)
    save(all.dist, file="data/all_dist.Rdata")
}

# Setup memory tracking
tracemem(all.met)
my.pre.process <- function(...){
    x <- pre.pamr(..., pre.process=pre.impute.knn, distmat=all.dist)
    trace.msg(4, tracemem(x))
    x
}

# Execute modelling
pred <- evaluate.modelling(proc, all.met, y, test.subset=cv,
                           pre.process=my.pre.process)

sets <- pre.pamr(all.met, y, cv[[1]])
ff <- fit(proc, sets$fit)
proc$predict.fun(ff[[1]], sets$test)


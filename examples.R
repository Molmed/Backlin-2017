source.all()
library(survival)
#library(predict)
citation("breastCancerUPP")

#===============================================================================
#   Section 3.1: A parallelized simulation study
#-------------------------------------------------------------------------------

set.seed(123)
source("generate_data.R")

# Setup parallelization
library(parallel)
options(mc.cores = 8)

# Define a serial random forest process
proc <- modelling.procedure("randomForest")

# Redefine the fitting function to parallelize it
parProc <- proc
parProc$fit.fun <- function(..., ntree=1000){
    # Divide the trees on all workers
    ntree <- round(rep(ntree/getOption("mc.cores"), getOption("mc.cores")))
    combine(mclapply(ntree, randomForest, ...))
}

system.time(model <- fit(proc, x, y))
system.time(parModel <- fit(parProc, x, y))


#===============================================================================
#   Section 3.2: Customized survival analysis modelling
#-------------------------------------------------------------------------------

source("http://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("breastCancerUPP")

#-------------------------------------------------------------------------------

# Load data
library(Biobase)
library(breastCancerUPP)
data(upp)
pheno <- pData(upp)
sample.idx <- pheno$treatment %in% 2
x <- t(exprs(upp))[sample.idx,]
y <- with(pheno[sample.idx,], Surv(t.rfs, e.rfs))

# Setup analysis
pre.pca <- function(x, y, fold){
    pca <- prcomp(x[na.fill(!fold, FALSE),,drop=FALSE])
    list(fit = pca$x,
         test = x[na.fill(fold, FALSE),] %*% pca$rotation)
}
proc <- modelling.procedure(
    fit.fun = function(x, y, nfeat)
        list(nfeat = nfeat,
             cox = coxph(y ~ ., data.frame(x[, 1:nfeat, drop=FALSE]))),
    predict.fun = function(fit, x)
        list(risk = predict(fit,
            data.frame(x[, 1:fit$nfeat, drop=FALSE]), type="risk")),
    param = list(nfeat = c(1, 2, 3, 5, 9, 15, 25)))

ho <- resample.holdout(y[,"status"], frac=.25, nrep=10)

# Run
perf <- evaluate.modelling(proc, x, y, ho, pre.pca)
ssubtree(perf, T, T, "error")

#----------------------------------------------------------------------[ kladd ]
sets <- pre.pca(x, y, ho[[1]])
my.fit <- fit(proc, sets$fit, y[!ho[[1]]])
pred <- predict.cox(my.fit[[1]], sets$test)
neg.harrell.C(y[na.fill(ho[[1]], FALSE)], pred)


#===============================================================================
#   Section 3.3: Methylation based subtyping
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------





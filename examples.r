#!/usr/bin/Rscript

#===============================================================================
#   This script contains the code examples in sections 2 and 3 of the paper.
#   The benchmarking study of section 4 can be found in `benchmark_setup.R`.
#-------------------------------------------------------------------------------

# Install and load required packages
required.pkg <- c("ElemStatLearn", "emil", "randomForest", "survival")
installed.pkg <- sapply(required.pkg, require, character.only = TRUE)
if(any(!installed.pkg)){
    install.packages(required.pkg[!installed.pkg])
}
options(digits = 3)


sum(sapply(c(fit, tune, evaluate), function(f) length(body(f))))
library(caret)
sum(sapply(c(train.default, caret:::nominalTrainWorkflow,
             caret:::looTrainWorkflow, caret:::adaptiveWorkflow),
           function(f) length(body(f))))


#===============================================================================
#   Section 2: The main example
#-------------------------------------------------------------------------------

library(ElemStatLearn)
data(prostate)
cv <- resample(method = "crossvalidation", y = prostate$lpsa,
               nfold = 3, nreplicate = 2)
result <- evaluate(procedure = "lasso",
                   x = prostate[1:8],
                   y = prostate$lpsa,
                   resample = cv,
                   pre_process = function(x, y, fold){
                       pre_split(x, y, fold) %>%
                       pre_scale %>%
                       pre_convert(x_fun = as.matrix)
                   })
get_performance(result)


#===============================================================================
#   Section 2.2: Resampling
#-------------------------------------------------------------------------------

cv <- resample(method = "crossvalidation", y = prostate$lpsa,
               nfold = 3, nreplicate = 2)
head(cv)


#===============================================================================
#   Section 2.3: Splitting and pre-processing 
#-------------------------------------------------------------------------------

# Piped pre-processing chain
prostate_split <- pre_split(x = prostate[1:8],
                            y = prostate$lpsa,
                            fold = cv[[1]]) %>%
                  pre_scale() %>%
                  pre_convert(x_fun = as.matrix)
print(prostate_split)

# Equivalent non-piped pre-processing chain
prostate_split <- pre_convert(
    pre_scale(
        pre_split(x = prostate[1:8], y = prostate$lpsa, fold = cv[[1]])
    ),
    x_fun = as.matrix)

# Example of a pre-processing function
print(pre_pca)

# Incorporation of pre-processing chain using list representation
result <- evaluate(procedure = "lasso", 
                   x = prostate[1:8], 
                   y = prostate$lpsa,
                   resample = cv, 
                   pre_process = list(pre_split, pre_scale,
                       function(data) pre_convert(data, x_fun = as.matrix)
                   ))


#===============================================================================
#   Section 2.4: Model fitting and testing
#-------------------------------------------------------------------------------

model <- fit(procedure = "lasso", x = prostate_split$fit$x,
                                  y = prostate_split$fit$y)
prediction <- predict(object = model, x = prostate_split$test$x)

head(get_prediction(result, resample = cv, format = "wide"))

rf <- modeling_procedure(method = "randomForest",
                         parameter = list(mtry = c(1, 3, 6)))
print(rf)
tuned_rf <- tune(rf, x = prostate[1:8], y = prostate$lpsa, resample = cv)
get_tuning(tuned_rf)
print(rf$fit_fun)

if(interactive()){
    debugonce(rf$fit_fun)
    rf_result <- evaluate(procedure = rf, x = prostate[1:8], y = prostate$lpsa,
                          resample = cv,
                          pre_process = list(pre_split, pre_scale))
}


#===============================================================================
#   Section 2.5: Downstream analysis
#-------------------------------------------------------------------------------

subtree(result, TRUE, "error")
select(result, Fold = TRUE, RMSE = "error")

internal_tuning <- select(result, Fold = TRUE, "model", "fit", function(x){
    data.frame(Lambda = x$lambda,
               TuningRMSE = x$cvm)
})
head(internal_tuning)

require(ggplot2)
ggplot(internal_tuning, aes(x = Lambda, y = TuningRMSE, group = Fold)) + 
    geom_line()


#===============================================================================
#   Section 2.6: Model comparison
#-------------------------------------------------------------------------------

# Compare regression methods
comparison <- evaluate(procedure = c(LASSO = "lasso", 
                                     RR = "ridge_regression"),
                       x = prostate[1:8],
                       y = prostate$lpsa,
                       resample = cv,
                       pre_process = function(x, y, fold){
                           pre_split(x, y, fold) %>%
                           pre_scale %>%
                           pre_convert(x_fun = as.matrix)
                       })
get_performance(comparison, format = "wide")

# Compare pre-processing chains
chains <- list(
    standard = function(x, y, fold){
        pre_split(x, y, fold) %>% pre_convert(x_fun = as.matrix)
    },
    pca = function(x, y, fold){
        pre_split(x, y, fold) %>% pre_pca
    }
)
comparison <- lapply(chains, function(my_chain){
    evaluate(procedure = "lasso", x = prostate[1:8], y = prostate$lpsa,
             resample = cv, pre_process = my_chain)
})
comparison %>%
    select(pre_process = TRUE, get_performance) %>%
    spread(pre_process, error)


#===============================================================================
#   Section 2.7: Scalability
#-------------------------------------------------------------------------------

#===============================================================================
#   Section 3.1: A parallelized simulation
#-------------------------------------------------------------------------------

# Set up the problem
x <- matrix(rnorm(100 * 10000), 100, 10000)
y <- gl(2, 50)
cv <- resample("crossvalidation", y, nfold = 8, nreplicate = 4)

# Evaluate the sequential solution
proc <- modeling_procedure("randomForest", parameter = list(ntree = 8000))
system.time(result_seq <- evaluate(procedure = proc, x = x, y = y,
                                   resample = cv))

# Evaluate the standard parallel solution
system.time(result_par1 <- evaluate(procedure = proc, x = x, y = y,
                                    resample = cv, .cores = 16))

# Set up and evaluate the alternative parallel solution
library("parallel")
options(mc.cores = 16)
par_proc <- proc
par_proc$fit_fun <- function(..., ntree){
    # Calculate how many trees each core needs compute
    nc <- getOption("mc.cores")
    ntree <- table(findInterval(1:ntree - 1, ntree / nc * 1:nc))

    # Fit the cores' forests
    forests <- mclapply(ntree, function(nt) randomForest(..., ntree = nt))

    # Combine the cores' forests into a single forest
    do.call(combine, forests)
}
system.time(result_par2 <- evaluate(procedure = par_proc, x = x, y = y,
                                    resample = cv))


#===============================================================================
#   Section 3.2: Survival modeling
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
require(survival)

# Load data
data(upp)
x <- data.frame(treatment = pData(upp)$treatment,
                t(exprs(upp)))
y <- with(pData(upp), Surv(t.rfs, e.rfs))

# Set up method
pre_cox_pca <- function(data){
    pca <- prcomp(data$fit$x[-1]) # Don't include the treatment feature
    data$fit$x <- data.frame(treatment = data$fit$x$treatment, pca$x)
    data$test$x <- data.frame(treatment = data$test$x$treatment, 
                              predict(pca, data$test$x[-1]))
    data
}
pca_cox <- modeling_procedure(
    method = "pca-cox",
    fit_fun = function(x, y, nfeat){
        terms <- c("treatment", sprintf("PC%i", seq_len(nfeat)))
        formula <- as.formula(sprintf("y ~ %s", 
                                      paste(terms, collapse=" + ")))
        coxph(formula, x)
    },
    predict_fun = predict_coxph,
    param = list(nfeat = c(0, 1, 2, 3, 5, 9, 15))
)

# Run
options(emil_max_indent = 4)
ho <- resample("holdout", y, nfold=3, test_fraction = 1/4)
result <- evaluate(procedure = pca_cox, x = x, y = y, resample = ho,
                   pre_process = list(pre_split, pre_cox_pca))


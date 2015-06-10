library(emil)
library("parallel")
options(mc.cores = 3)

# FIXME: Change to 16 cores and 8000 thousand trees and run on UPPMAX

x <- matrix(rnorm(100 * 10000), 100, 10000)
y <- gl(2, 50)
cv <- resample("crossvalidation", y, nfold = 4, nreplicate = 2)
proc <- modeling_procedure(
    method = "randomForest",
    fit_fun = function(...){
        gc()
        fit_randomForest(...)
    },
    parameter = list(ntree = 800)
)

Sys.sleep(3)


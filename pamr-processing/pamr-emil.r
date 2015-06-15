source("pamr-setup.r")
library(emil)

cv <- resample("crossvalidation", y, nfold=5, nreplicate=3)
procedure <- modeling_procedure("pamr",
    fit_fun = function(...){
        gc()
        fit_pamr(...)
    },
    parameter = list(n.threshold=3)
)

result <- evaluate(procedure, x, y, resample=cv,
                   pre_process = list(pre_split, pre_pamr),
                   .save=c(model=FALSE, prediction=FALSE, importance=FALSE))

Sys.sleep(3)


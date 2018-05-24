source("../get-geneexpression.R")
library("emil")

cv <- resample("crossvalidation", y, nrepeat = 3, nfold = 5)

procedure <- modeling_procedure(
    method = "pamr",
    fit_fun = function(...){
        gc()
        fit_pamr(...)
    },
    parameter = list(n.threshold = 10,
                     threshold = list(0:9))
)

result <- evaluate(procedure, x, y, resample = cv,
    pre_process = list(pre_split, pre_pamr),
    .save = c(model = FALSE, prediction = FALSE, importance = FALSE, error = TRUE),
    .verbose = TRUE
)

error <- get_performance(result)
print(error)

Sys.sleep(3)


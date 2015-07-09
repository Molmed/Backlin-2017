source("../get-sonar.r")
library(emil)

fit_LogitBoost <- function(x, y, nIter=ncol(x), ...){
    gc()
    nice_require("caTools")
    inner_procedure <- modeling_procedure(
        fit_fun = function(x, y, nIter){
            model <- caTools::LogitBoost(xlearn = x, ylearn = y, nIter = max(nIter))
            model$nIter <- nIter
            model
        },
        predict_fun = function(object, x, ...){
            lapply(object$nIter, function(i) predict(object, x, nIter = i))
        },
        error_fun = function(truth, prediction){
            sapply(prediction, function(p) error_rate(truth, p, allow_rejection=TRUE))
        },
        parameter = list(nIter = list(nIter))
    )
    if(length(nIter) > 1){
        cv <- resample("crossvalidation", y, nrepeat = 10, nfold = 10)
        result <- evaluate(inner_procedure, x, y, resample = cv,
                           .verbose = FALSE)
        error <- subtree(result, TRUE, "error")
        inner_procedure$parameter$nIter <- nIter[which.min(rowMeans(error))]
    }
    fit(inner_procedure, x, y)$model
}
predict_LogitBoost <- function(object, x, ...){
    list(prediction = predict(object, x, nIter = object$nIter))
}

procedure <- modeling_procedure(
    method = "LogitBoost",
    parameter = list(nIter = list(c(10, 20, 30))),
    error_fun = function(...) error_rate(..., allow_rejection=TRUE)
)

fold <- resample(method="holdout", y=Sonar$Class, nfold=1, 
                 test_fraction=.25)[[1]]

result <- evaluate(procedure, x = Sonar, y = "Class", resample=fold,
    pre_process = function(x, y, fold){
        pre_split(x, y, fold) %>%
        pre_convert(x_fun=as.matrix)
    },
    .save = c(model = FALSE, prediction = TRUE, error = FALSE)
)

Sys.sleep(3)


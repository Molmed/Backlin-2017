source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- replicate(3, createFolds(y, k = 5), simplify=FALSE)
trControl <- trainControl(
    method = "none",
    preProcOptions = list(k = 5),
    returnData = FALSE)
rf <- getModelInfo("parRF")$parRF
rf$fit <- function(x, y, wts, param, lev, classProbs, ...){
    randomForest(x, y, mtry = param$mtry, ...)
}

procedure <- modeling_procedure("caret",
    fit_fun = function(x, y, ...){
        gc()
        fit_caret(x=x, y=y, ...)
    },
    parameter = list(
        method = rf,
        preProcess = "knn",
        trControl = list(trControl),
    )
)

result <- evaluate(procedure, x, y, resample=cv,
                   pre_process = pre_split,
                   .save=c(model=FALSE, prediction=FALSE, importance=FALSE))

Sys.sleep(3)


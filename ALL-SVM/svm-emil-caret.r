source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nreplicate=2, nfold=3)

trControl <- trainControl(
    method = "repeatedcv",
    repeats = 2,
    number = 3,
    returnData = FALSE,
    allowParallel = FALSE)

svmPoly <- getModelInfo("svmPoly")$svmPoly
svmPoly$fit <- function( ...){
    gc()
    getModelInfo("svmPoly")$svmPoly$fit(...)
}

procedure <- modeling_procedure("caret",
    fit_fun = function(x, y, ...){
        gc()
        fit_caret(x=x, y=y, ...)
    },
    parameter = list(
        method = list(svmPoly),
        tuneGrid = list(data.frame(degree = 1:3, scale = .01)),
        trControl = list(trControl)
    )
)

result <- evaluate(procedure, x, y, resample=cv,
    pre_process = function(x, y, fold){
        pre_split(x=x, y=y, fold=fold) %>%
        pre_convert(x_fun = as.matrix)
    },
    .save=c(model=FALSE, prediction=FALSE, importance=FALSE, error = TRUE),
    .verbose = TRUE
)

Sys.sleep(3)


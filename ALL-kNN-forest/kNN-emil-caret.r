remove.values <- TRUE
source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nreplicate = 3, nfold = 5)

trControl <- trainControl(
    method = "none",
    preProcOptions = list(k = 5),
    returnData = FALSE,
    allowParallel = FALSE)

rf <- getModelInfo("parRF")$parRF
rf$fit <- function(x, y, wts, param, lev, classProbs, ...){
    gc()
    randomForest(x, y, mtry = param$mtry, ...)
}

procedure <- modeling_procedure("caret",
    parameter = list(
        method = list(rf),
        preProcess = "knnImpute",
        ntree = 100,
        tuneGrid = list(data.frame(mtry = floor(sqrt(ncol(x))))),
        trControl = list(trControl)
    )
)

result <- evaluate(procedure, x, y, resample = cv,
    pre_process = pre_split,
    .save = c(model = FALSE, prediction = FALSE, importance = FALSE, error = TRUE),
    .verbose = TRUE
)

error <- get_performance(result)

Sys.sleep(3)


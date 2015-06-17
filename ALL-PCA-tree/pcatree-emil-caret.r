source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nreplicate = 2, nfold = 3)

trControl <- trainControl(
    method = "repeatedcv",
    repeats = 2,
    number = 3,
    preProcOptions = list(pcaComp = 20),
    returnData = FALSE,
    allowParallel = FALSE)

rpart2 <- getModelInfo("rpart2")$rpart2
rpart2$fit <- function(...){
    gc()
    getModelInfo("rpart2")$rpart2$fit(...)
}

procedure <- modeling_procedure("caret",
    parameter = list(
        method = list(rpart2),
        preProcess = "pca",
        tuneGrid = list(data.frame(maxdepth = c(2,3,5))),
        trControl = list(trControl)
    )
)

result <- evaluate(procedure, x, y, resample=cv,
    pre_process = pre_split,
    .save=c(model=FALSE, prediction=FALSE, importance=FALSE, error = TRUE),
    .verbose = TRUE
)

error <- get_performance(result)

Sys.sleep(3)


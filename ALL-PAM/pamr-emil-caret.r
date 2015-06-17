source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nreplicate = 3, nfold = 5)

trControl <- trainControl(
    method = "repeatedcv",
    repeats = 3,
    number = 5,
    returnData = FALSE,
    allowParallel = FALSE)

pam <- getModelInfo("pam")$pam
pam$fit <- function(...){
    gc()
    getModelInfo("pam")$pam$fit(...)
}

procedure <- modeling_procedure(
    method = "caret",
    parameter = list(
        method = list(pam), 
        tuneGrid = list(data.frame(threshold = 0:9)),
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


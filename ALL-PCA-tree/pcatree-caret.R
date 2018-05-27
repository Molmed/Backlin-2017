set.seed(as.integer(commandArgs(trailing = TRUE)))
source("../get-geneexpression.R")
library("caret")

cv <- replicate(2, createFolds(y, k = 3), simplify = FALSE)
error <- matrix(NA, 2, 3)

trControl <- trainControl(
    method = "repeatedcv",
    repeats = 2,
    number = 3,
    preProcOptions = list(pcaComp = 20),
    returnData = FALSE,
    allowParallel = FALSE)

rpart2 <- getModelInfo("rpart2")$rpart2
rpart2$fit <- function(...) {
    gc()
    getModelInfo("rpart2")$rpart2$fit(...)
}

for (i in seq_along(cv)) {
    for (j in seq_along(cv[[i]])) {
        trainIndex <- unlist(cv[[i]][-j])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex, ], y[trainIndex],
                       method = rpart2,
                       preProcess = "pca",
                       tuneGrid = data.frame(maxdepth = c(2, 3, 5)),
                       trControl = trControl)
        prediction <- predict(model, x[testIndex, ])
        error[i, j] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

print(error)

Sys.sleep(3)


source("../get-geneexpression.R")
library(caret)

cv <- replicate(3, createFolds(y, k = 5), simplify = FALSE)
error <- matrix(NA, 3, 5)

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

for(i in seq_along(cv)){
    for(j in seq_along(cv[[i]])){
        trainIndex <- unlist(cv[[i]][-j])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = pam,
                       tuneGrid = data.frame(threshold = 0:9),
                       trControl = trControl)
        prediction <- predict(model, x[testIndex,])
        error[i, j] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

print(error)

Sys.sleep(3)


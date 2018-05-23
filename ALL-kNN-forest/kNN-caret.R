remove.values <- TRUE
source("../get-geneexpression.R")
library(caret)
library(randomForest) # Must import it here since caret fails to import it automatically

cv <- replicate(3, createFolds(y, k = 5), simplify = FALSE)
error <- matrix(NA, 3, 5)

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

for(i in seq_along(cv)){
    for(j in seq_along(cv[[i]])){
        trainIndex <- unlist(cv[[i]][-j])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = rf,
                       preProcess = "knnImpute",
                       tuneGrid = data.frame(mtry = floor(sqrt(ncol(x)))),
                       ntree = 100,
                       trControl = trControl)
        prediction <- predict(model, x[testIndex,])
        error[i, j] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

print(error)

Sys.sleep(3)


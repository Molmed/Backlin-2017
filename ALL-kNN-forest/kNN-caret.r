remove.values <- TRUE
source("../get-geneexpression.r")
library(caret)

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
    for(j in 2:length(cv[[i]])){
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = rf,
                       preProcess = "knnImpute",
                       tuneGrid = data.frame(mtry = floor(sqrt(ncol(x)))),
                       ntree = 100,
                       trControl = trControl)
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)


source("../get-geneexpression.r")
library(caret)

cv <- replicate(2, createFolds(y, k = 3), simplify=FALSE)
error <- matrix(NA, 2, 3)

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

for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = svmPoly,
                       tuneGrid = data.frame(degree = 1:3, scale=.01),
                       trControl = trControl)
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)


source("../get-geneexpression.r")
library(caret)

cv <- replicate(3, createFolds(y, k = 5), simplify=FALSE)
error <- matrix(NA, 3, 5)
trControl <- trainControl(
    method = "repeatedcv",
    number = 5,
    repeats = 3,
    preProcOptions = list(pcaComp = 20),
    returnData = FALSE)

for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        gc()
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = "rpart2",
                       preProcess = "pca",
                       grid = data.frame(maxdepth = c(2,3,5)),
                       trControl = trControl)
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)


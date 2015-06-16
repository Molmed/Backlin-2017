source("../get-geneexpression.r")
library(caret)

cv <- replicate(2, createFolds(y, k = 3), simplify=FALSE)
error <- matrix(NA, 2, 3)
trControl <- trainControl(
    method = "repeatedcv",
    number = 3,
    repeats = 2,
    preProcOptions = list(pcaComp = 20),
    returnData = FALSE)

for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        gc()
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        system.time(
        model <- train(x, y,
                       method = "rpart2",
                       preProcess = "pca",
                       tuneGrid = data.frame(maxdepth = c(2,3,5)),
                       trControl = trControl,
                       subset = trainIndex)
        )
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)


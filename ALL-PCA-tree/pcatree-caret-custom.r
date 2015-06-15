source("../get-geneexpression.r")
library(caret)

my_rpart <- getModelInfo("rpart")$rpart
my_rpart$fit <- function(x, ...){
    pca <- prcomp(x)
    model <- getModelInfo("rpart")$rpart$fit(x = pca$x[,1:20], ...)
    pca$x <- NULL # to save memory
    list(pca = pca,
         rpart = model)
}
my_rpart$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    predict(modelFit$rpart,
            newdata = as.data.frame(predict(modelFit$pca, newdata)[,1:20]),
            type="class")
}
my_rpart$grid <- function(...){
    data.frame(cp = 0.01)
}


cv <- replicate(3, createFolds(y, k = 5), simplify=FALSE)
error <- matrix(NA, 3, 5)
for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        gc()
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = my_rpart,
                       trControl = trainControl(method = "none",
                                                returnData = FALSE))
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)



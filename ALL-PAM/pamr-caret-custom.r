source("../get-geneexpression.r")
library(caret)

nsc <- getModelInfo("pam")$pam
nsc$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
    my.data <- list(x=t(x), y=y)
    fit <- pamr.train(my.data, n.threshold=10)
    fit.cv <- pamr.cv(fit, my.data)
    list(fit = fit, cv = fit.cv,
         threshold = with(fit.cv, median(threshold[error == min(error)])))
}
nsc$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL){
    pamr.predict(modelFit$fit, t(newdata), type="class",
                 threshold=modelFit$threshold)
}

cv <- replicate(3, createFolds(y, k = 5), simplify=FALSE)
error <- matrix(NA, 3, 5)
for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        gc()
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = nsc,
                       tuneGrid = data.frame(threshold = 0),
                       trControl = trainControl(method = "none",
                                                returnData = FALSE))
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}

Sys.sleep(3)


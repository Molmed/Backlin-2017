source("../get-sonar.r")
library(caret)

lbFuncs <- list(
    library = "caTools",
    loop = function(grid) {
        loop <- grid[which.max(grid$nIter),,drop = FALSE]
        submodels <- grid[-which.max(grid$nIter),,drop = FALSE]
        submodels <- list(submodels)
        list(loop = loop, submodels = submodels)
    },
    type = "Classification",
    parameters = data.frame(parameter = 'nIter',
                            class = 'numeric',
                            label = '# Boosting Iterations'),
    grid = function(x, y, len = NULL){
        data.frame(nIter = 1 + ((1:len)*10))
    },
    fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) {
        LogitBoost(as.matrix(x), y, nIter = param$nIter)
    },
    predict = function(modelFit, newdata, preProc = NULL, submodels = NULL){
        out <- predict(modelFit, newdata, type="class")
        if(!is.null(submodels)) {
            tmp <- out
            out <- vector(mode = "list", length = nrow(submodels) + 1)
            out[[1]] <- tmp

            for(j in seq(along = submodels$nIter)) {
                out[[j+1]] <- predict(modelFit,
                                      newdata,
                                      nIter = submodels$nIter[j])
            }
        }
        out
    },
    prob = NULL,
    sort = function(x) x
)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10)

inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
model <- train(Class ~ ., data = training,
               method = lbFuncs,
               tuneLength = 3,
               trControl = fitControl)

testing  <- Sonar[-inTraining,]
prediction <- predict(model, testing)

Sys.sleep(3)


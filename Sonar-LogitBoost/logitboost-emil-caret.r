source("../get-sonar.r")
library(caret)
library(emil)

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
        gc()
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

trControl <- trainControl(
    method = "repeatedcv",
    repeats = 10,
    number = 10)

procedure <- modeling_procedure(
    method = "caret",
    parameter = list(
        method = list(lbFuncs),
        tuneGrid = list(data.frame(nIter = c(10, 20, 30))),
        trControl = list(trControl)
    )
)

fold <- resample(method="holdout", y=Sonar$Class, nfold=1,
                 test_fraction=.25)[[1]]

result <- evaluate(procedure, Sonar, "Class", resample=fold,
    pre_process = function(x, y, fold){
       pre_split(x, y, fold) %>%
       pre_convert(x_fun=as.matrix)
    },
    .save = c(model = FALSE, prediction = TRUE, error = FALSE)
)

Sys.sleep(3)


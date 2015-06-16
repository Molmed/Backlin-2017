source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nreplicate = 3, nfolds = 5)

procedure <- modeling_procedure("caret",
    fit_fun = function(x, y, ...){
        gc()
        fit_caret(x=x, y=y, ...)
    },
    parameter = list(
        method = "rpart2",
        preProcess = "pca",
        grid = list(data.frame(maxdepth = c(2,3,5))),
        trControl = list(trainControl(
            method = "repeatedcv",
            number = 5,
            repeats = 3,
            preProcOptions = list(pcaComp = 20),
            returnData = FALSE
        ))
    )
)

result <- evaluate(procedure, x, y, resample=cv,
                   pre_process = pre_split,
                   .save=c(model=FALSE, prediction=FALSE, importance=FALSE))

Sys.sleep(3)


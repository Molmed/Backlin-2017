source("../get-geneexpression.r")
library(caret)
library(emil)

cv <- resample("crossvalidation", y, nfold=5, nreplicate=3)

procedure <- modeling_procedure("caret",
    fit_fun = function(x, y, ...){
        gc()
        fit_caret(x=x, y=y, ...)
    },
    parameter = list(
        method = "svmPoly",
        tuneGrid = list(data.frame(degree = 1:3, scale = 1, C = 1)),
        trControl = list(trainControl(
            method = "repeatedcv",
            number = 5,
            repeats = 3,
            returnData = FALSE
        ))
    )
)

result <- evaluate(procedure, x, y, resample=cv,
                   pre_process = function(x, y, fold){
                       pre_split(x=x, y=y, fold=fold) %>%
                       pre_convert(x_fun = as.matrix)
                   },
                   .save=c(model=FALSE, prediction=FALSE, importance=FALSE))

Sys.sleep(3)


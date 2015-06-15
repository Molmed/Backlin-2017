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
        method = "pam", 
        tuneLength = 10,
        trControl = list(trainControl(
            method = "repeatedcv",
            number = 5,
            repeats = 3,
            returnData = FALSE
        ))
    )
)

result <- evaluate(procedure, x, y, resample=cv,
                   pre_process = pre_split,
                   .save=c(model=FALSE, prediction=FALSE, importance=FALSE))

Sys.sleep(3)


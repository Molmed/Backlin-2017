source("../get-geneexpression.r")
library(emil)

procedure <- modeling_procedure(
    method = "svm",
    fit_fun = function(x, y, degree=1, scale=1, C=1){
        gc()
        nice_require("kernlab")
        kernlab::ksvm(x = x, y = y,
             kernel = kernlab::polydot(degree = degree,
                                       scale = scale,
                                       offset = 1),
             C = C)
    },
    predict_fun = function(object, x){
        list(prediction = kernlab::predict(object, x))
    },
    parameter = list(degree = 1:3)
)

cv <- resample("crossvalidation", y, nreplicate=2, nfold=3)
result <- evaluate(procedure, x, y, resample=cv, .verbose=TRUE)

Sys.sleep(3)


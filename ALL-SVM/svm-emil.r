source("../get-geneexpression.r")
library(emil)

cv <- resample("crossvalidation", y, nrepeat = 2, nfold = 3)

procedure <- modeling_procedure(
    method = "svm",
    fit_fun = function(x, y, degree = 1){
        gc()
        nice_require("kernlab")
        kernlab::ksvm(x = x, y = y, C=1,
             kernel = kernlab::polydot(degree = degree,
                                       scale = .01,
                                       offset = 1))
    },
    predict_fun = function(object, x){
        list(prediction = kernlab::predict(object, x))
    },
    parameter = list(degree = 1:3)
)

system.time(fit(procedure, x, y))

result <- evaluate(procedure, x, y, resample = cv,
    pre_process = function(x, y, fold){
        pre_split(x = x, y = y, fold = fold) %>%
        pre_convert(x_fun = as.matrix)
    },
    .save = c(model = FALSE, prediction = FALSE, importance = FALSE, error = TRUE),
    .verbose = TRUE
)

error <- get_performance(result)
print(error)

Sys.sleep(3)

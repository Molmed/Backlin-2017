source("../get-geneexpression.r")
library(emil)

procedure <- modeling_procedure(
    method = "randomForest",
    fit_fun = function(x, y, mtry = floor(sqrt(ncol(x))), ntree, ...){
        gc()
        fit_randomForest(x, y, mtry = mtry, ntree = ntree, ...)
    },
    parameter = list(ntree = 100)
)

x_dist <- dist(x)

cv <- resample("crossvalidation", y, nreplicate=3, nfold=5)
result <- evaluate(procedure, x, y, resample=cv, .verbose=TRUE,
    pre_process = function(x, y, fold){
        pre_split(x, y, fold) %>%
        pre_impute_knn(k = 5, distance_matrix = x_dist)
    })

Sys.sleep(3)





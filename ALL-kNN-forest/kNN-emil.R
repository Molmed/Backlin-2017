remove.values <- TRUE
source("../get-geneexpression.R")
library("emil")

cv <- resample("crossvalidation", y, nrepeat = 3, nfold = 5)

procedure <- modeling_procedure(
    method = "randomForest",
    fit_fun = function(x, y, mtry = floor(sqrt(ncol(x))), ntree, ...){
        gc()
        fit_randomForest(x, y, mtry = mtry, ntree = ntree, ...)
    },
    parameter = list(ntree = 100)
)

x_dist <- dist(x)

result <- evaluate(procedure, x, y, resample = cv,
    pre_process = function(x, y, fold){
        pre_split(x, y, fold) %>%
        pre_impute_knn(k = 5, distance_matrix = x_dist)
    },
    .save = c(model = FALSE, prediction = FALSE, importance = FALSE, error = TRUE),
    .verbose = TRUE
)

error <- get_performance(result)
print(error)

Sys.sleep(3)





source("../get-geneexpression.r")
library(emil)

cv <- resample("crossvalidation", y, nfold=5, nreplicate=3)
procedure <- modeling_procedure("rpart",
    fit_fun = function(...){
        gc()
        fit_rpart(...)
    },
    parameter = list(maxdepth = c(2,3,5))
)

result <- evaluate(procedure, x, y, resample=cv,
    pre_process = function(x, y, fold){
        pre_split(x, y, fold) %>%
        pre_pca(ncomponent = 20) %>%
        pre_convert(x_fun = as.data.frame)
    },
    .save=c(model=FALSE, prediction=FALSE, importance=FALSE)
)

get_performance(result)

Sys.sleep(3)


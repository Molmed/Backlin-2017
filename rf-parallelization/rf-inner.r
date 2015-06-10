source("rf-setup.r")

proc$fit_fun <- function(x, y, ntree){
    gc()
    nice_require("randomForest")

    # Calculate how many trees each core needs compute
    nc <- getOption("mc.cores")
    ntree <- table(findInterval(1:ntree - 1, ntree / nc * 1:nc))

    # Fit the cores' forests
    forests <- mclapply(ntree, function(nt)
        randomForest::randomForest(x, y, ntree = nt))

    # Combine the cores' forests into a single forest
    do.call(randomForest::combine, forests)
}
result_par2 <- evaluate(procedure = proc, x = x, y = y, .cores=1, resample = cv)

Sys.sleep(3)


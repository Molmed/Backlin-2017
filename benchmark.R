#!/usr/bin/Rscript

arg <- commandArgs()
arg <- arg[-(1:which(arg == "--args"))]

framework <- arg[1]
stopifnot(framework %in% c("caret", "predict"))
require(caret)
require(emil)
require(class)

algorithm <- arg[2]
stopifnot(algorithm %in% c("glmnet", "pamr", "randomForest"))
require(algorithm, character.only=TRUE)

load("../data/common.Rdata")
n.feat <- as.integer(arg[3])
load(paste0("../data/met_", n.feat, ".Rdata"))

pre.proc <- pre.split
if(framework == "caret"){
    levels(y) <- gsub("\\W", "", levels(y))

    cv.control <- trainControl(
        method = if(algorithm == "pamr") "none" else "repeatedcv",
        number = attr(cv[[1]], "nfold"),
        repeats = attr(cv[[1]], "nrep"),
        returnData = FALSE,
        verboseIter = TRUE,
        allowParallel = algorithm == "randomForest")

    params <- switch(algorithm,
        glmnet = list(method = "glmnet", nlambda=30),
        pamr = list(method = "pam"),
        randomForest = list(method = "rf", ntree = 16*400))

    paramGrid <- switch(algorithm,
        glmnet = {
            a <- 0:4/4
            l <- lapply(a, function(a) glmnet(x, y, "multinomial", alpha=a, nlambda=30)$lambda)
            do.call(rbind, mapply(data.frame, .alpha=a, .lambda=l, SIMPLIFY=FALSE))
        },
        pamr = data.frame(.threshold = 10),
        randomForest = data.frame(.mtry = floor(ncol(x)^(2/3:5)))
    )

    proc <- modeling.procedure("caret", param = c(params,
        list(trControl = list(cv.control), tuneGrid = list(paramGrid))))

    if(algorithm == "glmnet"){
        proc$predict.fun <- function(object, x, ...){
            list(pred = factor(
                predict(object$finalModel, newx = x,
                        s = object$bestTune$.lambda, type="class", ...),
                levels=levels(y)))
        }
    }
    if(algorithm == "randomForest"){
        require(doMC)
        registerDoMC(16)
    }

} else if(framework == "predict"){
    proc <- modeling.procedure(
        method = algorithm,
        param = switch(algorithm,
            glmnet = list(alpha = 0:4/4, nlambda=30),
            pamr = list(threshold = 10),
            randomForest = list(ntree = 16*400, mtry = floor(ncol(x)^(2/3:5)))
        )
    )
    if(algorithm == "pamr"){
        pre.proc <- pre.pamr
    }
    if(algorithm == "randomForest"){
        require(parallel)
        options(mc.cores = 16)
        proc$fit.fun <- function(..., ntree){
            nc <- getOption("mc.cores")
            ntree <- table(findInterval(seq_len(ntree)-1, ntree/nc * seq_len(nc)))
            forests <- mclapply(ntree, function(nt) randomForest(..., ntree=nt))
            do.call(combine, forests)
        }
    }
}

computation.time <- system.time(
    result <- evaluate.modeling(proc, x, y, pre.process=pre.proc, resample=cv)
)

save(computation.time, result,
    file=sprintf("results/%s_%s_%i.Rdata", framework, algorithm, n.feat))


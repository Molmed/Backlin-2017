#! /usr/bin/Rscript

arg <- commandArgs()
arg <- arg[-(1:which(arg == "--args"))]

framework <- arg[1]
stopifnot(framework %in% c("caret", "predict"))
do.call(library, list(framework))

algorithm <- arg[2]
stopifnot(algorithm %in% c("glmnet", "randomForest"))
do.call(library, list(algorithm))
library(class)
if(algorithm == "randomForest"){
    library(doMC)
    registerDoMC(8)
}


n.feat <- as.integer(arg[3])
load(paste0("data/met_", n.feat, ".Rdata"))
load("data/common.Rdata")

if(framework == "caret"){
    levels(y) <- gsub("\\W", "", levels(y))

    cv.control <- trainControl(method = "repeatedcv",
                               index = lapply(cv, function(x) which(!x)),
                               indexOut = lapply(cv, which),
                               savePredictions = TRUE,
                               classProbs = TRUE)

    # Create tuning grid
    if(algorithm == "glmnet"){
        tmp.fit <- glmnet(x, y, "multinomial")
        paramGrid <- expand.grid(.alpha=1, .lambda=tmp.fit$lambda)
        rm(tmp.fit)
        computation.time <- system.time(
            result <- train(x, y, "glmnet",
                trControl = cv.control, tuneGrid=paramGrid)
        )
    } else if(algorithm == "randomForest"){
        paramGrid <- data.frame(.mtry = floor(sqrt(ncol(x))))
        # Default in the randomForest package
        computation.time <- system.time(
            result <- train(x, y, "rf", ntree=1000,
                            trControl = cv.control, tuneGrid=paramGrid)
        )
    }


} else if(framework == "predict"){

    if(algorithm == "randomForest"){
        design.para_rf <- function(...) {
            foreach(i=1:8, .combine=combine) %dopar% {
                randomForest(x, y, ntree=125)
            }
        }
        predict.para_rf <- getFromNamespace("predict.rf", "predict")
    }

    computation.time <- system.time(
        result <- batch.predict(x, y,
            c(glmnet="gln", randomForest="para_rf")[algorithm],
            test.subset=cv, .verbose=TRUE)
    )
}

save(computation.time, result,
    file=sprintf("results/%s_%s_%i.Rdata", framework, algorithm, n.feat))


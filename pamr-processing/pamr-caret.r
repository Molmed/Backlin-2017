source("pamr-setup.r")
library(caret)

cv <- replicate(3, createFolds(y, k = 5), simplify=FALSE)
error <- matrix(NA, 3, 5)
i <- j <- 2
for(i in seq_along(cv)){
    for(j in 2:length(cv[[i]])){
        gc()
        trainIndex <- unlist(cv[[i]][-c(1,j)])
        testIndex <- cv[[i]][[j]]
        model <- train(x[trainIndex,], y[trainIndex],
                       method = "pam",
                       trControl = trainControl(returnData = FALSE))
        prediction <- predict(model, x[testIndex,])
        error[i, j-1] <- 1 - postResample(prediction, y[testIndex])["Accuracy"]
        rm(trainIndex, testIndex, model, prediction)
    }
}
#trControl <- trainControl(method = "pam")

cat("job completed\n")
Sys.sleep(3)

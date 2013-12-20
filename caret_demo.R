#===============================================================================
#
#   Examples from the Caret paper
#   Building Predictive Models in R Using the caret Package
#   Max Kuhn, Journal of Statistical Software 28:5, 2008
#
#-------------------------------------------------------------------------------

load("descr.RData")
load("mutagen.RData")


# Section 3, Data preparation

library(caret)
set.seed(1)
inTrain <- createDataPartition(mutagen, p = 3/4, list = FALSE)

trainDescr <- descr[inTrain,]
testDescr <- descr[-inTrain,]
trainClass <- mutagen[inTrain]
testClass <- mutagen[-inTrain]

(prop.table(table(mutagen)))
(prop.table(table(trainClass)))

#---:

ncol(trainDescr)
descrCorr <- cor(trainDescr)
descrCorr[is.na(descrCorr)] <- 0 # CB add
highCorr <- findCorrelation(descrCorr, 0.90) 
trainDescr <- trainDescr[, -highCorr]
testDescr <- testDescr[, -highCorr]
ncol(trainDescr) # Result missmatch 653 vs 650 (run vs paper)

#---:

xTrans <- preProcess(trainDescr)
trainDescr <- predict(xTrans, trainDescr)
testDescr <- predict(xTrans, testDescr)

#---:

bootControl <- trainControl(number = 200)
system.time({
    set.seed(2)
    svmFit <- train(trainDescr, trainClass,
                    method = "svmRadial", tuneLength = 5,
                    trControl = bootControl, scaled = FALSE)
})
svmFit

#---:

gbmGrid <- expand.grid(.interaction.depth = (1:5) * 2,
                       .n.trees = (1:10)*25, .shrinkage = .1)
system.time({
    set.seed(2)
    gbmFit <- train(trainDescr, trainClass, method = "gbm",
                    trControl = bootControl, verbose = FALSE,
                    bag.fraction = 0.5, tuneGrid = gbmGrid)
})


#===============================================================================
#
#   Examples from ?train
#
#-------------------------------------------------------------------------------

#######################################
## Classification Example

data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]

knnFit1.time <- system.time({
    knnFit1 <- train(TrainData, TrainClasses,
                     method = "knn",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = trainControl(method = "cv"))
})

knnFit2.time <- system.time({
    knnFit2 <- train(TrainData, TrainClasses,
                     method = "knn",
                     preProcess = c("center", "scale"),
                     tuneLength = 10,
                     trControl = trainControl(method = "boot"))
})

#######################################
## Regression Example

library(mlbench)
data(BostonHousing)

lmFit.time <- system.time({
    lmFit <- train(medv ~ . + rm:lstat,
                   data = BostonHousing,
                   "lm")
})

# My version
library(predict)
cv <- resample.crossval(BostonHousing$medv, 10, 10)
cv.control <- trainControl(method = "repeatedcv",
                           index = lapply(cv, function(x) which(!x)),
                           indexOut = lapply(cv, which))
lmFit.time <- system.time({
    lmFit <- train(medv ~ . + rm:lstat,
                   data = BostonHousing,
                   "lm",
                   trControl = cv.control)
})
lmPred.time <- system.time({
    pred <- batch.predict(BostonHousing[-14], BostonHousing[[14]],
                          list(ols=list(formula = y ~ . + rm:lstat)),
                          cv)
})

library(rpart)
rpartFit <- train(medv ~ .,
                  data = BostonHousing,
                  "rpart",
                  tuneLength = 9)

library(MASS)
nnetFit.time <- system.time({
    nnetFit <- train(TrainData, TrainClasses,
                     method = "nnet",
                     preProcess = "range",
                     tuneLength = 2,
                     trace = FALSE,
                     maxit = 100)
})


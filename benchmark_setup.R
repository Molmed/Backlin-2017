#!/usr/bin/Rscript

for(d in c("data", "results", "runcontrol", "plots", "memstats"))
    dir.create(d, showWarnings=FALSE)

required.pkg <- c("caret", "doMC", "glmnet", "pamr", "predict", "randomForest")
installed.pkg <- rownames(installed.packages())
for(pkg in setdiff(required.pkg, installed.pkg))
    install.packages(pkg)


#------------------------------------o
#   Create bite size datasets to speed up loading

n.feat <- round(10^seq(2, 5, by=.5))
data.files <- c("data/common.Rdata", sprintf("data/met_%i.Rdata", n.feat))
data.file.missing <- !sapply(data.files, file.exists)

if(any(data.file.missing)){
    library(predict)
    if(file.exists("data/all_methylation.Rdata")){
        load("data/all_methylation.Rdata")
    } else {
        source("download_methylation.R")
    }

    sample.idx <- all.pheno$subtype %in% c("T-ALL", "HeH", "t(12;21)")
    y <- factor(all.pheno$subtype[sample.idx])
    cv <- resample.crossval(y, 4, 4)
    save(sample.idx, y, cv, n.feat, file="data/common.Rdata")

    all.met <- all.met[sample.idx,1:max(n.feat)]
    for(nf in n.feat[data.file.missing[-1]]){
        x <- na.fill(all.met[,1:nf], .5)
        save(x, file=sprintf("data/met_%i.Rdata", nf))
    }
}


#--------------------------------------o
#   Figure out what runs to compute

sec2time <- function(x){
    x <- round(x)
    sprintf("%i-%02i:%02i:%02i", x %/% (24*60*60),
            x %% (24*60*60) %/% (60*60), x %% (60*60) %/% 60, x %% 60)
}
runs <- data.frame(
    framework = gl(2, length(n.feat)*3, labels=c("caret", "predict")),
    algorithm = gl(3, length(n.feat), labels=c("glmnet", "pamr", "randomForest")),
    dimension = n.feat,
    max.time = sec2time(60 * (15 + 4*60 * n.feat/max(n.feat))))
runs$name <- with(runs, sprintf("%s_%s_%i", framework, algorithm, dimension))
runs$id <- gsub("([a-zA-Z])[a-zA-Z]*_", "\\1", runs$name)

# Remove all runs that have been completed
runs <- runs[!paste0(runs$name, ".Rdata") %in% dir("results"),]
runs <- runs[order(runs$max.time, decreasing=FALSE),]
runs <- runs[runs$dimension < 10000,]

#--------------------------------------o
#   Create batch scripts and launch


batch.script <- with(runs, paste0("#!/bin/sh -l
#SBATCH -A b2010028
#SBATCH -p node -N 1
#SBATCH -t ",max.time,"
#SBATCH --qos=b2010028_4nodes
#SBATCH -J ",id,"
#SBATCH --output=runcontrol/",name,".out
#SBATCH --error=runcontrol/",name,".err

./catmem.sh ",name," &
R -f benchmark.R --vanilla --args ",framework," ",algorithm," ",dimension,"
"))

for(i in seq_along(batch.script)){
    f <- paste0("runcontrol/", runs$name[i], ".sh")
    cat(batch.script[i], file=f)
    system(paste0("sbatch ", f))
}


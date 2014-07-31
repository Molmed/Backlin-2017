#!/usr/bin/Rscript

#===============================================================================
#   Benchmarking against the caret package
#
#   This script sets up and launches the benchmarking study on a slurm cluster,
#   such as the Uppsala Multidisciplinary Center for Advanced Computational
#   Science (UPPMAX) where it was originally run.
#
#   Run this script in the repo root.
#-------------------------------------------------------------------------------

# This is the folder to which all results are saved
run.folder <- "run1"


#------------------------------------o
#   Install packages

required.pkg <- c("caret", "data.table", "doMC", "emil", "glmnet", "pamr",
                  "randomForest")
installed.pkg <- rownames(installed.packages())
for(pkg in setdiff(required.pkg, installed.pkg))
    install.packages(pkg)


#------------------------------------o
#   Create bite size datasets to speed up loading

dir.create("data", showWarnings = FALSE)
n.feat <- round(10^seq(2, 5, by=.5))
data.files <- c("data/common.Rdata", sprintf("data/met_%i.Rdata", n.feat))
data.file.missing <- !sapply(data.files, file.exists)

if(any(data.file.missing)){
    require(emil)
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


#------------------------------------o
#   Setup the run

if(!file.exists(run.folder)){
    dir.create(run.folder)
    setwd(run.folder)
    system("ln -s ../catmem.sh .")
    system("ln -s ../benchmark.R .")
} else {
    setwd(run.folder)
}

for(d in c("runcontrol", "memstats", "results"))
    dir.create(d, showWarnings=FALSE)


#--------------------------------------o
#   Figure out what runs to compute

require(data.table)
sec2time <- function(x){
    x <- round(x)
    sprintf("%i-%02i:%02i:%02i", x %/% (24*60*60),
            x %% (24*60*60) %/% (60*60), x %% (60*60) %/% 60, x %% 60)
}
runs <- data.table(
    framework = gl(2, length(n.feat)*3, labels=c("caret", "emil")),
    algorithm = gl(3, length(n.feat), labels=c("glmnet", "pamr", "randomForest")),
    dimension = n.feat,
    max.time = sec2time(60*60*c(1+23*n.feat/10000,
                                seq(.1, 1, length.out=length(n.feat)),
                                1+23*n.feat/10000)
    )
)
runs$name <- with(runs, sprintf("%s_%s_%i", framework, algorithm, dimension))
runs$id <- gsub("([a-zA-Z])[a-zA-Z]*_", "\\1", runs$name)
runs$mem.interval <- ifelse(runs$algorithm == "pamr", 1, 10)

# Remove all runs that have been completed
runs <- runs[!paste0(name, ".Rdata") %in% dir("results")]
# And runs taking too long
runs <- runs[grepl("^[0-1]", max.time)]
# Sort the runs as you like
runs <- runs[order(dimension == 100, max.time, decreasing=TRUE)]


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

./catmem.sh ",name," ",mem.interval," &
R -f benchmark.R --vanilla --args ",framework," ",algorithm," ",dimension,"
"))

for(i in seq_along(batch.script)){
    f <- paste0("runcontrol/", runs$name[i], ".sh")
    cat(batch.script[i], file=f)
    system(paste0("sbatch ", f))
}


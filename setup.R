
#------------------------------------o
#   Make directory structure and download data

dir.create("data", showWarnings=FALSE)
dir.create("results", showWarnings=FALSE)
dir.create("runcontrol", showWarnings=FALSE)

source("download_methylation.R")


#------------------------------------o
#   Install required packages

required.pkg <- list(CRAN = c("caret", "doMC", "glmnet", "predict", "randomForest"),
                     BioConductor = "GEOquery")
installed.pkg <- rownames(installed.packages())

for(pkg in setdiff(required.pkg$CRAN, installed.pkg))
    install.packages(pkg)
if(any(!required.pkg$BioConductor %in% installed.pkg)){
    source("http://bioconductor.org/biocLite.R")
    for(pkg in setdiff(required.pkg$BioConductor, installed.packages))
        biocLite(pkg)
}


#------------------------------------o
#   Create bite size datasets to speed up loading

method.n.feat <- list(      glmnet = round(10^seq(2, 5, by=.5)),
                      randomForest = round(10^seq(2, 4, by=1/3)))
n.feat <- unique(unlist(method.n.feat))

data.files <- sprintf("data/met_%i.Rdata", n.feat)
data.file.missing <- !sapply(data.files, file.exists)

if(any(data.file.missing)){
    library(predict)
    library(analyse450k)
    load.450k.data("all")
    sample.idx <- all.pheno$subtype %in% c("T-ALL", "HeH", "t(12;21)")
    y <- factor(all.pheno$subtype[sample.idx])
    cv <- resample.crossval(y, 5, 3)
    all.met <- t(all.met[1:max(n.feat),sample.idx])

    for(nf in n.feat[data.file.missing]){
        x <- na.fill(all.met[,1:nf], .5)
        save(x, file=sprintf("data/met_%i.Rdata", nf))
    }
    save(sample.idx, y, cv, method.n.feat, file="data/common.Rdata")
}


#--------------------------------------o
#   Figure out what runs to compute

runs <- data.frame(framework = rep(c("caret", "predict"), each=length(unlist(method.n.feat))),
                   stack(method.n.feat)[2:1])
names(runs)[2:3] <- c("algorithm", "dimension")

# Estimate maximum allowed computation time (from earlier tests)
runs$max.time <- 5*60 +
    c(60, 120, 360, 3600, 20000, 2e5, NA,
      rep(NA, 7),
      200, 200, 200, 400, 900, 2600, 8200,
      rep(NA, 7))
runs$name <- with(runs, sprintf("%s_%s_%i", framework, algorithm, dimension))

# Remove all runs that have been completed
runs <- runs[!is.na(runs$max.time) &
             !paste0(runs$name, ".Rdata") %in% dir("results"),]
runs <- runs[order(runs$max.time, decreasing=TRUE),]


#--------------------------------------o
#   Create batch scripts and launch

sec2time <- function(x) sprintf("%i-%02i:%02i:%02i",
    x %/% (24*60*60), x %% (24*60*60) %/% (60*60), x %% (60*60) %/% 60, x %% 60)

batch.script <- with(runs, paste0("#! /bin/bash -l
#SBATCH -A b2010028
#SBATCH -p node -N 1
#SBATCH -t ",sec2time(max.time),"
#SBATCH --qos=b2010028_4nodes
#SBATCH -J ",gsub("([a-z])[a-z]*_", "\\1", name),"
#SBATCH --output=runcontrol/",name,".out
#SBATCH --error=runcontrol/",name,".err

./catmem.sh ",name," &
R -f modeling.R --vanilla --args ",framework," ",algorithm," ",dimension,"
"))

for(i in seq_along(batch.script)){
    f <- paste0("runcontrol/", runs$name[i], ".sh")
    cat(batch.script[i], file=f)
    system(paste0("sbatch ", f))
}


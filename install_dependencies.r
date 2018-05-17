#!/usr/bin/Rscript

deps <- read.csv("dependencies.csv")

missing_cran_deps <- setdiff(deps$package[deps$repo == "cran"],
                             rownames(installed.packages()))
if(any(missing_cran_deps)){
    install.packages(missing_cran_deps)
}

missing_bioc_deps <- setdiff(deps$package[deps$repo == "bioc"],
                             rownames(installed.packages()))
if(any(missing_bioc_deps)){
  if(!require("Biobase")){
    source("https://bioconductor.org/biocLite.R")
    biocLite(ask=FALSE)  
  }
  biocLite(pkg)
}

rm(deps, missing_bioc_deps, missing_cran_deps)

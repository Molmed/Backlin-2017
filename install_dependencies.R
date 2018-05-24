#!/usr/bin/Rscript

if(!require("devtools")){
  install.packages("devtools")
  stopifnot(require("devtools"))
}

deps <- read.csv("dependencies.csv", stringsAsFactors = FALSE)

install_dep <- function(pkg, version, repo, lib.loc=NULL){
  if(is.character(lib.loc)){
    dir.create(lib.loc, showWarnings=FALSE)
  }

  ip <- installed.packages(lib.loc=lib.loc)
  
  if(!pkg %in% row.names(ip)){
    if(repo == "bioc"){
      source("https://bioconductor.org/biocLite.R")
      biocLite(pkg, lib.loc=lib.loc)
      return(TRUE)
    }
    install_version(pkg, version = version, type = "source", lib.loc=lib.loc)
    return(TRUE)
  }

  have_version <- ip[pkg, "Version"]
  if(version == have_version){
    return(TRUE)
  }

  if(is.null(lib.loc)) {
    return(FALSE)  # Don't overwrite existing package in default lib.loc
  }

  install_version(pkg, version = version, type = "source", lib.loc=lib.loc)
}

for(i in 1:nrow(deps)){
  install_dep(deps$package[i], deps$version[i], deps$repo[i], lib.loc=NULL)
  install_dep(deps$package[i], deps$version[i], deps$repo[i], lib.loc="vendor")
}

missing_packages <- setdiff(deps$package, row.names(installed.packages()))
if(length(missing_packages) > 0){
  stop(paste("failed to install packages:", paste(missing_packages, collapse=", ")))
} else {
  message("All dependencies installed successfully")  
}

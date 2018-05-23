#!/usr/bin/Rscript

if(!require("devtools")){
  install.packages("devtools")
}

deps <- read.csv("dependencies.csv", stringsAsFactors = FALSE)

install_dep <- function(pkg, version, repo){
  ip <- installed.packages()
  
  if(!pkg %in% row.names(ip)){
    if(repo == "bioc"){
      source("https://bioconductor.org/biocLite.R")
      biocLite(pkg)
      return()
    }
    install_version(pkg, version = version, type = "source")
    return()
  }
  
  have_version <- ip[pkg, "Version"]
  if (version != have_version && repo != "bioc") {
    msg <- sprintf(
      "Found package %s version %s, want %s. Do you want to replace currently installed version? [y/n/Q] ",
      pkg, have_version, version
    )
    ans <- tolower(readline(msg))
    if(ans == "q"){
      stop("aborting")  
    }
    if(ans == "y"){
      install_version(pkg, version = version, type = "source")
      return()
    }
    
    warning(sprintf("Keeping package %s version %s. Code might not work.", pkg, version))
  }
}

for(i in 1:nrow(deps)){
  install_dep(deps$package[i], deps$version[i], deps$repo[i])
}

missing_packages <- setdiff(deps$package, row.names(installed.packages()))
if(length(missing_packages) > 0){
  stop(paste("failed to install packages:", paste(missing_packages, collapse=", ")))
} else {
  message("All dependencies installed successfully")  
}

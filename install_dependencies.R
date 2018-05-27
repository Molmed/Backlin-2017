#!/usr/bin/Rscript

repos <- "https://ftp.acc.umu.se/mirror/CRAN/"

.libPaths("vendor")
dir.create("vendor", showWarnings = FALSE)
dir.create("vendor_cache", showWarnings = FALSE)

if (!require("devtools")) {
  install.packages("devtools", repos=repos)
  stopifnot(require("devtools"))
}

deps <- read.csv("dependencies.csv", stringsAsFactors = FALSE)

install_dep <- function(pkg, version, repo, lib = NULL) {
  if (is.character(lib)) {
    dir.create(lib, showWarnings = FALSE)
  }

  ip <- installed.packages(lib = lib)

  if (!pkg %in% row.names(ip)) {
    if (repo == "bioc") {
      source("https://bioconductor.org/biocLite.R")
      biocLite(pkg, lib = lib, destdir="vendor_cache")
      return(TRUE)
    }
    install_version(pkg, version = version, type = "source", lib = lib, repos = repos, destdir="vendor_cache")
    return(TRUE)
  }

  have_version <- ip[pkg, "Version"]
  if (version == have_version) {
    return(TRUE)
  }

  if (is.null(lib)) {
    return(FALSE)  # Don't overwrite existing package in default lib
  }

  install_version(pkg, version = version, type = "source", lib = lib, repos = repos, destdir="vendor_cache")
}

for (i in 1:nrow(deps)) {
  install_dep(deps$package[i], deps$version[i], deps$repo[i])
}

missing_packages <- setdiff(deps$package, row.names(installed.packages()))
if (length(missing_packages) > 0) {
  stop(paste("failed to install packages:", paste(missing_packages, collapse = ", ")))
}

message("All dependencies installed successfully")


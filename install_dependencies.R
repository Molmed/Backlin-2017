#!/usr/bin/Rscript

repos <- "https://ftp.acc.umu.se/mirror/CRAN/"

dir.create("vendor", showWarnings = FALSE)
dir.create("vendor_cache", showWarnings = FALSE)

.libPaths("vendor")

if (!require("devtools", lib = "vendor")) {
  install.packages("devtools", lib = "vendor", repos=repos)
  stopifnot(require("devtools", lib = "vendor"))
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
  if (version == have_version || repo == "bioc") {
    return(TRUE)
  }

  if (is.null(lib)) {
    return(FALSE)  # Don't overwrite existing package in default lib
  }

  install_version(pkg, version = version, type = "source", lib = lib, repos = repos, destdir="vendor_cache")
}

for (i in 1:nrow(deps)) {
  install_dep(deps$package[i], deps$version[i], deps$repo[i], lib = "vendor")
}

missing_packages <- setdiff(deps$package, row.names(installed.packages()))
if (length(missing_packages) > 0) {
  stop(paste("failed to install packages:", paste(missing_packages, collapse = ", ")))
}

message("All dependencies installed successfully")


Developer friendly and computationally efficient predictive modeling without information leakage: The `emil` package for R
=======================
Christofer L Bäcklin, Mats G Gustafsson

Original publication: TBA

This repository contains all code required to run the examples, benchmarking test, and producing the figures of the paper.

System requirements
-------------------
The code is written for a Unix or Linux operating system with R version 3.0.2 or later, but can be modified to run under Windows fairly easily. The following packages are required, but installed automatically: `BioBase`, `breastCancerUpp`, `caret`, `cforest`, `data.table`, `doMC`, `emil`, `GEOquery`, `glmnet`, `gtools`, `pamr`, `parallel`, and `randomForest`.

Instructions
------------
The most convenient way to run the analysis is to clone the repo to your computer and run the files as shown below (commands for unix/linux).

    git clone git@github.com:Molmed/Backlin-2014.git
    cd Backlin-2014
    R -f benchmark_setup.R

The file `examples.R` contain the code examples presented in the paper.

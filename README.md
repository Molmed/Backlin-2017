Developer Friendly Predictive Modelling with the `predict` Package for R
=======================
Christofer L Bäcklin, Mats G Gustafsson

Original publication: TBA

This repository contains all code examples and code used to perform the benchmarking test (section 4, figure #).

System requirements
-------------------
The code is written for a Unix or Linux operating system with R version 3.0.1 or later, but can be modified to run under Windows fairly easily. Packages `randomForest`, `glmnet` and `doMC` are required, but are installed automatically.

Instructions
------------
The most convenient way to run the analysis is to clone the repo to your computer and run the files as shown below (commands for unix/linux).

    git clone git@github.com:Molmed/Nordlund-2013.git
    cd Backlin-2014
    R -f setup.R
    R -f benchmark.R

The file `examples.R` contain the code examples presented in the paper.
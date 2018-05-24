Developer Friendly and Computationally Efficient Predictive Modeling without Information Leakage: The `emil` Package for R
=======================
Christofer L Bäcklin, Mats G Gustafsson.

This repository contains all code required to run the examples,
benchmarking test, and producing the figures of the original publication.

## Code examples from the paper
The code examples of section 2 and 3 of the paper can be found in the script `examples.R`.

## Benchmark study
Clone, install dependencies, and run in one go:

```
git clone git@github.com:Molmed/Backlin-2017.git
cd Backlin-2017
make benchmark
```

The code is written for MacOS or Linux operating systems with R version
3.5.0 and package dependencies listed in `dependencies.csv`.

In addition to R dependencies you also need Python (preferably version 3.5.2) to
enable memory profiling, implemented using [syrupy](https://github.com/jeetsukumaran/Syrupy).

The code should work fine with other versions of R, Python, and package dependencies too
but is not _guaranteed_ to do so.
If you encounter errors please post an issue or a even better a pull-request with a fix.



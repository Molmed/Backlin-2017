#!/usr/bin/Rscript

header <-
"#===============================================================================
#
#   This scripts holds all code required to replicate the results of the paper
#   \"Developer Friendly and Computationally Efficient Predictive Modeling
#   without Information Leakage: The emil Package for R\" by Christofer L Bäcklin
#   and Mats G Gustafsson (2018).
#   
#   It is essentially all the files of the GitHub repository
#   https://github.com/Molmed/Backlin-2017 joined into one.
#   
#-------------------------------------------------------------------------------

"

files <- dir(".", "(py|r|sh)$", recursive=TRUE)
files <- unique(c("examples.r", files[order(grepl("/", files))]))
output <- "emil_v3_replication.txt"
cat(header, file=output, append=FALSE)

for(f in files){
    cat("#===============================================================================\n",
        file = output, append = TRUE)
    cat("#", f, "\n\n", file = output, append = TRUE)
    system(paste("cat", f, ">>", output))
    cat("\n", file = output, append = TRUE)
}


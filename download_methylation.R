#!/opt/apps/R/3.0.1/bin/Rscript

#===============================================================================
#   Download and import methylation into R
#
#   This data consist of beta values normalized with peak based correction
#   (Dedeurwaerder et al. 2011).
#
#   Beware that you need approximately 20 GB of RAM to run this script
#   efficiently, since the gzipped text file is read into a 6.9 GB data frame
#   (x) and processed into a 3.4 GB numeric matrix as the final result, leaving
#   some computational headroom.
#-------------------------------------------------------------------------------

options(stringsAsFactors=FALSE)
if(!exists("met.annot"))
    load("data/annotations.Rdata")

in.file <- "data/processed.txt.gz"
if(!file.exists(in.file))
    download.file("ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE49nnn/GSE49031/suppl/GSE49031_processed.txt.gz", in.file)

columns <- unlist(read.csv(in.file, nrow=1L, sep="\t", header=FALSE))
x <- read.csv(in.file, nrow=485577L, header=TRUE, sep="\t",
    colClasses=c(ID_REF="character", Beta="numeric", Pval="numeric")[sub(".* ", "", columns)])

met.data <- do.call(cbind, x[grep("Beta", names(x))])
met.pval <- do.call(cbind, x[grep("Pval", names(x))])
rm(x)
met.data[met.pval > .01] <- NA
rm(met.pval)

save(met.data, file="data/methylation.Rdata", compress=FALSE)


#===============================================================================
#   Download and process phenotypes
#
#   This dataset was originally uploaded for a study by Nordlund & Bäcklin
#   et al. (2013).
#-------------------------------------------------------------------------------

require(GEOquery)

in.file <- "data/GSE49031_series_matrix.txt.gz"
if(!file.exists(in.file))
    download.file("ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE49nnn/GSE49031/matrix/GSE49031_series_matrix.txt.gz", in.file)
gse <- getGEO(filename=in.file, destdir="data")


sample.type.patterns <- c(
    control   = "^MethDNA(Pos|Neg)",
    reference = "^(\\d{3}_[BT]|cd34_e1)",
    diagnosis = "^ALL_\\d+$",
    replicate = "^ALL_\\d+rep\\d",
    remission = "^Constitutional_\\d+",
    `relapse 1` = "^ALL_\\d+r1", 
    `relapse 2` = "^ALL_\\d+r2")

char.fields <- c(disease.state = "characteristics_ch1",
    immunophenotype = "characteristics_ch1.1",
    subtype = "characteristics_ch1.2",
    cell.type = "characteristics_ch1.3")

met.pheno <- with(phenoData(gse)@data, data.frame(
    id = sub(" .*$", "", description),
    geo.accession = as.character(geo_accession),
    title = as.character(title),
    sample.type = factor(NA, levels=names(sample.type.patterns)),
    lapply(char.fields, function(x) factor(sub("^.*: ", "", get(x)))),
    tissue = factor(source_name_ch1),
    stringsAsFactors=FALSE))

for(i in seq(sample.type.patterns))
    met.pheno$sample.type[grep(sample.type.patterns[i], met.pheno$id)] <-
        names(sample.type.patterns)[i]

idx <- sapply(met.pheno, function(x) "NA" %in% levels(x))
met.pheno[idx] <- lapply(met.pheno[idx], function(x){
    factor(as.character(x), levels=setdiff(levels(x), "NA"))
})


#===============================================================================
#   References:
#
#    - Dedeurwaerder S, Defrance M, Calonne E, Denis H, Sotiriou C, Fuks F:
#      Evaluation of the Infinium Methylation 450K technology.
#      Epigenomics 2011, 3:771–784.
#
#    - Nordlund & Bäcklin et al.: Genome-wide signatures of differential DNA
#      methylation in pediatric acute lymphoblastic leukemia, 
#      Genome Biology 2013, 14:r105.
#-------------------------------------------------------------------------------

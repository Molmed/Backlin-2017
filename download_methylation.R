#!/usr/bin/Rscript

#===============================================================================
#   Download and import methylation into R
#
#   This dataset was originally uploaded for a study by Nordlund & Bäcklin
#   et al. (2013). It contains DNA methylation data in the form of beta values
#   normalized with peak based correction (Dedeurwaerder et al. 2011).
#
#   Beware that you need approximately 20 GB of RAM to run this script
#   efficiently, since the gzipped text file is read into a 6.9 GB data frame
#   (x) and processed into a 3.4 GB numeric matrix as the final result, leaving
#   some computational headroom.
#
#   References:
#
#    - Nordlund & Bäcklin et al.: Genome-wide signatures of differential DNA
#      methylation in pediatric acute lymphoblastic leukemia, 
#      Genome Biology 2013, 14:r105.
#
#    - Dedeurwaerder S, Defrance M, Calonne E, Denis H, Sotiriou C, Fuks F:
#      Evaluation of the Infinium Methylation 450K technology.
#      Epigenomics 2011, 3:771–784.
#
#-------------------------------------------------------------------------------

if(!require(GEOquery)){
    source("http://bioconductor.org/biocLite.R")
    biocLite(GEOquery)
}
options(stringsAsFactors=FALSE)


#----------------------------------------------------------------[ Annotations ]

dir.create("data")
in.file <- "data/SupplementalTable1.txt.gz"
if(!file.exists(in.file))
    download.file("http://genomebiology.com/imedia/6808530410895336/supp1.gz",
                  "data/SupplementalTable1.txt.gz")

met.annot <- read.csv(in.file,
    header=TRUE, sep="\t", nrows=485577,
    colClasses=c("character", "factor", "integer", "character",
                 "integer", "character", "character", "factor",
                 "character", "character", "character", "character",
                 "factor", "character", "character",
                 # probe filtering
                 "integer", "integer", "integer",
                 # Histone marks
                 "integer", "integer", "integer", "integer", 
                 "integer", "integer", "integer", "integer",
                 # DMCS
                 "integer", "integer", "integer", "integer", 
                 "integer", "integer", "integer", "integer", 
                 "integer", "integer", "integer", "integer"))
# Make the chromosome name a factor manually to get the order right
met.annot$CHR <- factor(met.annot$CHR, levels=c(1:22, c("X", "Y")))
met.annot$CHROMOSOME_36 <- factor(met.annot$CHROMOSOME_36,
    levels=c(levels(met.annot$CHR), "MULTI"))
met.annot[16:38] <- lapply(met.annot[16:38], as.logical)

save(met.annot, file="data/methylation_annotation.Rdata")


#----------------------------------------------------------------[ Methylation ]

in.file <- "data/processed.txt.gz"
if(!file.exists(in.file))
    download.file("ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE49nnn/GSE49031/suppl/GSE49031_processed.txt.gz", in.file)

columns <- unlist(read.csv(in.file, nrow=1L, sep="\t", header=FALSE))
x <- read.csv(in.file, nrow=485577L, header=TRUE, sep="\t",
    colClasses=c(ID_REF="character", Beta="numeric", Pval="numeric")[sub(".* ", "", columns)])

all.met <- do.call(cbind, x[grep("Beta", names(x))])
all.met.pval <- do.call(cbind, x[grep("Pval", names(x))])
rm(x)
all.met[all.met.pval > .01] <- NA
rm(all.met.pval)
# Remove unreliable probes, see Nordlund & Backlin 2013 for details
all.met <- t(all.met[met.annot$analyzed,])


#-----------------------------------------------------------------[ Phenotypes ]

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

all.pheno <- with(phenoData(gse)@data, data.frame(
    id = sub(" .*$", "", description),
    geo.accession = as.character(geo_accession),
    title = as.character(title),
    sample.type = factor(NA, levels=names(sample.type.patterns)),
    lapply(char.fields, function(x) factor(sub("^.*: ", "", get(x)))),
    tissue = factor(source_name_ch1),
    stringsAsFactors=FALSE))

for(i in seq(sample.type.patterns))
    all.pheno$sample.type[grep(sample.type.patterns[i], all.pheno$id)] <-
        names(sample.type.patterns)[i]

idx <- sapply(all.pheno, function(x) "NA" %in% levels(x))
all.pheno[idx] <- lapply(all.pheno[idx], function(x){
    factor(as.character(x), levels=setdiff(levels(x), "NA"))
})

save(all.met, all.pheno, file="data/all_methylation.Rdata")
rm(met.annot)



#----------------------------------------------------------------[ Choose runs ]

# Paths to run folders
runs <- c("run_140325", "run_140329", "run_140417")


#------------------------------------------------------------------[ Load data ]

require(emil)
require(gtools)
require(data.table)

extend <- function(x, inc=.04)
    x + c(-1, 1)*diff(x)*inc
nice.axis <- function(..., las = 1, lwd = par("lwd"), lwd.ticks = lwd, lend = 2){
    axis(..., las = las, lwd = 0, lwd.ticks = lwd.ticks, lend = 1)
    if(lwd) {
        args <- c(list(...), list(lwd = lwd, lwd.ticks = 0, lend = lend))
        args$labels <- FALSE
        do.call(axis, args)
    }
}
vlines <- function (x, lend = 1, ...)
    segments(x, par("usr")[3], x, par("usr")[4], lend = lend, ...)
hlines <- function (y, lend = 1, ...)
    segments(par("usr")[1], y, par("usr")[2], y, lend = lend, ...)


logs <- lapply(runs, function(r){
    l <- mixedsort(dir(paste0(r, "/memstats"), full.names=TRUE))
    names(l) <- sub("^.*/(\\w+)\\.log", "\\1", l)
    l
})
mems <- lapply(logs, function(l) do.call(rbind, lapply(l, function(f){
    tab <- readLines(f)
    if(length(tab) == 44) return(NULL)

    time <- as.integer(as.POSIXct(tab[seq(1, length(tab), by=44)]))
    time <- time[-1] - time[1]
    mem <- as.integer(sub("^.*?(\\d+) kB$", "\\1", tab[2])) -
        as.integer(sub("^.*?(\\d+) kB$", "\\1", tab[seq(3, length(tab), by=44)]))
    mem <- pmax(mem[-1] - mem[1], 1)
    algorithm <- sub("^.*(glmnet|pamr|randomForest).*$", "\\1", f)
    data.table(framework = factor(sub("^.*(caret|emil).*$", "\\1", f),
                                  levels=c("caret", "emil")),
               algorithm = factor(algorithm, levels=c("glmnet", "pamr", "randomForest")),
               dimension = as.integer(sub("^.*?(\\d+).*$", "\\1", f)),
               time=log10(time),
               mem=cummax(log10(mem))
    )
})))
runs.completed <- lapply(logs, function(l) data.table(
    framework = sub(".*(caret|emil).*", "\\1", l),
    algorithm = sub(".*(glmnet|pamr|randomForest).*", "\\1", l),
    dimension = as.integer(sub("^.*?(\\d+).*$", "\\1", l)),
    completed = sub("^.*/(.*)\\.log$", "\\1.Rdata", l) %in%
        dir(sub("memstats.*$", "results", l[1]))
))
fold.start.times <- lapply(logs, lapply, function(l){
    ll <- readLines(sub("memstats/(\\w+)\\.log$", "runcontrol/\\1.out", l))
    fold.start.times <- as.POSIXct(
        grep("^\\d+ \\w+ \\d{2}:\\d{2}      Extracting", ll, value=TRUE),
        format="%d %b %H:%M")
    sapply(fold.start.times, difftime, fold.start.times[1], units="mins")
    #as.integer(difftime(tail(fold.start.times, 1), head(fold.start.times, 1), units="mins"))
})

for(r in seq_along(mems)){
    mems[[r]]$replicate <- r
    runs.completed[[r]]$replicate <- r
}
mems <- do.call(rbind, mems)
runs.completed <- do.call(rbind, runs.completed)

# Now source `plot_draw.R`


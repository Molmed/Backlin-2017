#!/usr/bin/Rscript

#===============================================================================
#   This script exports a plot of the benchmarking results.
#   Note that `plot_setup.R` must be run before this script can be executed.
#-------------------------------------------------------------------------------

orientation <- c("portrait", "landscape")[1]
export.to.file <- TRUE

if(!export.to.file){
    if(orientation == "portrait"){
        X11(, 14/cm(1), 21/cm(1))
    } else {
        X11(, 21/cm(1), 14/cm(1))
    }
}


#------------------------------------------------------------------[ Draw plot ]

# Color palette
pal <- cbind(
    caret = hsv(seq(1/6, 0, len=7), .5, seq(.9, .8, length.out=7)),
    emil = hsv(seq(1/6, .5, len=7), .5, seq(.9, .8, length.out=7))
)
rownames(pal) <- sort(unique(mems$dimension))

# Set up device
if(names(dev.cur()) %in% c("X11", "X11cairo", "quartz")){
    plot.new()
} else {
    if(orientation == "portrait"){
        pdf("plots/benchmark.pdf", 14/cm(1), 21/cm(1))
    } else {
        #pdf("plots/benchmark_landscape.pdf", 21/cm(1), 14/cm(1))
        png("plots/benchmark_landscape.png", 21, 14, "cm", res=150)
    }
}
if(orientation == "portrait"){
    layout(matrix(1:6, 3, byrow=TRUE))
} else {
    layout(matrix(1:6, 2))
}
par(cex=1, ps=8, mar=c(1.3, 3.2, 1.5, 2.3), oma=c(1,1,1,0),
    las=1, tcl=-.3, mgp=c(1,.3,0))

time.ticks <- log10(c(1, s=1:12*5, min=1:12*5*60, h=1:4*5*60^2))
time.labels <- rep(NA, length(time.ticks))
time.labels[time.ticks %in% log10(c(1, 10, 15, 60, 5*60, 60^2, 5*60^2))] <-
    c("1s", "10 s", "15 s", paste(c(1,5), rep(c("min", "h"), each=2)))

mem.ticks <- log10(1:9*10^rep(3:8, each=9))
mem.labels <- rep(NA, length(mem.ticks))
mem.labels[mem.ticks == floor(mem.ticks)] <- 
    paste(10^(0:2), rep(c("MB", "GB"), each=3))

counter <- 0
for(a in levels(mems$algorithm)[3:1]){
    counter <- counter + 1
    if(nrow(mems[algorithm == a]) > 0){
        xl <- range(mems[algorithm == a, time])
        yl <- extend(range(mems[algorithm == a, mem]), .1)
        tt <- ifelse(findInterval(time.ticks, xl) == 1, time.ticks, NA)
        mt <- ifelse(findInterval(mem.ticks, yl) == 1, mem.ticks, NA)
    }
    for(f in levels(mems$framework)){
        if(nrow(mems[framework == f & algorithm == a]) == 0){
            plot.new()
        } else {
            plot(0, 0, type="n", xlim=xl, ylim=yl,
                las=1, axes=FALSE, ann=FALSE)
            for(d in unique(mems[framework == f & algorithm == a, dimension])){
                for(r in seq_along(runs)){
                    with(mems[framework == f & algorithm == a & dimension == d & replicate == r], {
                        if(length(time) > 1) lines(time, mem, col=pal[as.character(d),f])

                        if(!all(runs.completed[algorithm == a & dimension == d, completed])){
                            for(r in seq_along(runs)){
                                m <- mems[algorithm == a & framework == f & dimension == d & replicate == r]
                                if(nrow(m) > 0) with(m,
                                     points(approx(time, mem,
                                            log10(60*fold.start.times[[r]][[paste(f, a, d, sep="_")]][-1])),
                                    pch="|", col=pal[as.character(d), f], cex=.7))
                            }
                        }
                    })
                }
            }

            if(counter == 1){
                mtext(f,
                    c(portrait=3, landscape=2)[orientation],
                    c(portrait=.6, landscape=3.1)[orientation],
                    cex=par("cex"), las=0, font=2)
            }
            if((orientation == "portrait" && counter == 3) ||
               (orientation == "landscape" && f == "emil"))
                mtext("Computation time", 1, 1.3, cex=par("cex"))
            if((orientation == "portrait" && f == "caret") ||
               (orientation == "landscape" && counter == 1))
                mtext("Memory usage (cumulative max)", 2, 2.2, cex=par("cex"), las=0)
            if(f == "caret"){
                mtext(switch(a,
                    glmnet="glmnet, sequential",
                    pamr = "pamr, sequential",
                    randomForest="random forest, parallel (16 cores)"),
                    c(portrait=2, landscape=3)[orientation],
                    c(portrait=3.1, landscape=.6)[orientation],
                    cex=par("cex"), las=0, font=2)
            }
            hlines(mt, col=c("#00000022", "#00000011")[1+is.na(mem.labels)])
            axis(2, mt, mem.labels, lwd=0)
            vlines(log10(c(10, 60, 10*60, 60*60, 10*60*60)), col=rep(c("#00000011")), lty=1)
            nice.axis(1, time.ticks, time.labels)

            tmp <-merge(mems[framework == f & algorithm == a,
                             list(time=tail(time,1), mem=tail(mem,1)), by=list(dimension, replicate)],
                        runs.completed[framework == f & algorithm == a], by=c("dimension", "replicate"))
            with(tmp, points(time, mem, col=pal[as.character(dimension),f],
                             cex=.6, pch=ifelse(completed, 19, 4)))
            with(tmp[,list(time=max(time), mem=mean(mem)), by=list(dimension)],
                 text(time, mem, dimension, pos=4, xpd=TRUE))
        }
    }
}
par(new=TRUE, mar=rep(0, 4), oma=rep(0, 4), fig=c(0:1,0:1))
plot(0, 0, type="n", axes=FALSE, ann=FALSE)
if(!names(dev.cur()) %in% c("X11", "X11cairo", "quartz"))
    dev.off()


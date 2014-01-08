library(predict)
library(analyse450k)
library(gtools)
library(foreach)
library(data.table)

logs <- mixedsort(dir("memstats", full.names=TRUE))
mems <- foreach(f = logs, .combine=rbind) %do% {
    tab <- readLines(f)
    if(length(tab) == 44) return(NULL)

    time <- as.integer(as.POSIXct(tab[seq(1, length(tab), by=44)]))
    time <- time[-1] - time[1]
    mem <- as.integer(sub("^.*?(\\d+) kB$", "\\1", tab[2])) -
        as.integer(sub("^.*?(\\d+) kB$", "\\1", tab[seq(3, length(tab), by=44)]))
    mem <- mem[-1] - mem[1]
    data.table(framework = factor(sub("^.*(caret|predict).*$", "\\1", f), levels=c("caret", "predict")),
               algorithm = factor(sub("^.*(glmnet|pamr|randomForest).*$", "\\1", f), levels=c("glmnet", "pamr", "randomForest")),
               dimension = as.integer(sub("^.*?(\\d+).*$", "\\1", f)),
               time=log10(time), mem=log10(mem))
}
runs.completed <- data.table(
    framework = sub(".*(caret|predict).*", "\\1", logs),
    algorithm = sub(".*(glmnet|pamr|randomForest).*", "\\1", logs),
    dimension = as.integer(sub("^.*?(\\d+).*$", "\\1", logs)),
    completed = sub("^memstats/(.*)\\.log$", "\\1.Rdata", logs) %in% dir("results")
)

X11(, 14/cm(1), 21/cm(1))
pal <- c(caret="#ee8800", predict="#000080")
{
    # Set up device
    if(names(dev.cur()) %in% c("X11", "X11cairo", "quartz")){
        plot.new()
    } else {
        pdf("plots/memory.pdf", 14/cm(1), 21/cm(1))
        #png("plots/memory.png", 14, 14, "cm", res=150)
    }
    par(mfrow=c(3,2), cex=1, ps=8, mar=c(1.3, 3.2, 1.5, 2.3), oma=c(1,1,1,0),
        las=1, tcl=-.3, mgp=c(1,.3,0))

    time.ticks <- log10(c(s=1:12*5, min=1:12*5*60, h=1:4*5*60^2))
    time.labels <- rep(NA, length(time.ticks))
    time.labels[time.ticks %in% log10(c(10, 15, 60, 5*60, 60^2, 5*60^2))] <-
        c("10 s", "15 s", paste(c(1,5), rep(c("min", "h"), each=2)))

    mem.ticks <- log10(1:9*10^rep(3:8, each=9))
    mem.labels <- rep(NA, length(mem.ticks))
    mem.labels[mem.ticks == floor(mem.ticks)] <- 
        paste(10^(0:2), rep(c("MB", "GB"), each=3))

    for(a in levels(mems$algorithm)){
        if(nrow(mems[algorithm == a]) > 0){
            xl <- range(mems[algorithm == a, time])
            yl <- log10(c(50e3, 30e6)) #range(mems[algorithm == a, mem])
            tt <- ifelse(findInterval(time.ticks, xl) == 1, time.ticks, NA)
            mt <- ifelse(findInterval(mem.ticks, yl) == 1, mem.ticks, NA)
        }
        for(f in levels(mems$framework)){
            if(nrow(mems[framework == f & algorithm == a]) == 0){
                plot.new()
            } else {
                plot(0, 0, type="n", xlim=xl, ylim=yl,
                    las=1, axes=FALSE, ann=FALSE)

                if(a == "glmnet"){
                    mtext(f, 3, .6, cex=par("cex"), font=2)
                } else {
                    mtext("Computation time", 1, 1.3, cex=par("cex"))
                }
                if(f == "caret"){
                    mtext("Memory usage", 2, 2.1, cex=par("cex"), las=0)
                    mtext(switch(a,
                        glmnet="glmnet, sequential",
                        pamr = "pamr, sequential",
                        randomForest="random forest, parallel (16 cores)"),
                        2, 3.1, cex=par("cex"), las=0, font=2)
                }
                hlines(mt, col=c("#dddddd", "#eeeeee")[1+is.na(mem.labels)])
                axis(2, mt, mem.labels, lwd=0)
                vlines(log10(c(60, 60*60)), col=rep(c("#dddddd")), lty=2)
                axis(1, time.ticks, time.labels, lend=1)

                for(d in unique(mems[framework == f & algorithm == a, dimension])){
                    with(mems[framework == f & algorithm == a & dimension == d], {
                        if(length(time) > 1) lines(time, mem, col=sprintf("%s55", pal[f]))
                        points(tail(time, 1), tail(mem, 1), col=pal[f], cex=.6,
                               pch=if(runs.completed[framework == f & algorithm == a & dimension == d, completed]) 19 else 1)
                        text(tail(time, 1), tail(mem, 1), d, col=pal[f], pos=4, xpd=TRUE)
                    })
                }
            }
        }
    }
    if(!names(dev.cur()) %in% c("X11", "X11cairo", "quartz"))
        dev.off()
}


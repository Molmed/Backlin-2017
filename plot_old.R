#-----------------------------------o
#   Plot

load("comparison.Rdata")
pt <- fit.time$predict
load("comparison2.Rdata")
ct <- fit.time$caret
n.feat <- 10^seq(2, 5, by=.5)

tim <- cbind(c(sapply(ct[1:4], "[", 1), rep(NA, 3)), sapply(pt, "[", 1))


pal <- c("#ee8800", "navy")
{
    #pdf("plots/scaling.pdf", 8/cm(1), 8/cm(1))
    png("plots/scaling.png", 8, 8, "cm", res=150)
    if(is.null(dev.list()))
        X11(, 8/cm(1), 8/cm(1))
    if(names(dev.cur()) %in% c("X11", "X11cairo", "quartz"))
        plot.new()
    par(ps=8, mar=c(2.3, 3.2, .5, .7), tcl=-.3, las=1, mgp=c(1,.3,0))

    feat.tick <- log10(n.feat)
    feat.labels <- c(100, NA, "1,000", NA, "10,000", NA, "100,000")
    l <- log10(c(1:12*5, 1:12*12*5^2, 1:5*12^2*5^3))
    ll <- log10(c(15, 20, 60, 5*60, 60^2, 5*60^2))

    plot(0, 0, type="n", xlim=range(log10(n.feat)), ylim=log10(c(15, 5*60*60)),
        axes=FALSE, ann=FALSE)
    mtext("Number of features", 1, 1.3, cex=par("cex"))
    mtext("Computation time", 2, 2.0, las=0)

    l <- l[findInterval(l, par("usr")[3:4]) == 1]
    hlines(l, col=c("#eeeeee", "#dddddd")[1+c(F, diff(diff(l)) > 0, F)])

    #legend("bottomright", c("Caret", "Predict"), lwd=1, col=pal, bg="white", box.col="white")
    axis(1, feat.tick, feat.labels)
    axis(2, ll, c("15 s", "20 s", "1 min", "5 min", "1 h", "5 h"), lwd=0)
    #    at=log10(c(15, 20, 60, 5*60, 60*60, 5*60*60)), las=1)
    for(i in 1:2){
        lines(log10(n.feat), log10(tim[,i]), col=pal[i])
        points(log10(n.feat), log10(tim[,i]), col=pal[i], pch=19, cex=.6)
    }
    # Computation was interrupted for caret
    lines(log10(n.feat[4:5]), log10(c(tim[4,1], 214*60)), col=pal[1], lty=2)
    points(log10(n.feat[5]), log10(214*60), col=pal[1], cex=.6)
    text(log10(n.feat[c(5,7)]), log10(c(214*60, tim[7,2])),
         c("Caret", "Predict"), col=pal, pos=2)
    if("pdf" %in% names(dev.cur()))
         text(log10(n.feat[5]), log10(214*60), "Interrupted", col=pal, pos=4, cex=.5)

    if(!names(dev.cur()) %in% c("X11", "X11cairo", "quartz"))
        dev.off()
}

matplot(log10(n.feat), log10(tim), type="l", axes=FALSE, ylim=c(0, ))




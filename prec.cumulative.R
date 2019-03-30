prec.cumulative <- function(time.series=c("Boreas.xts", "c1.xts", "hhm.xts"), time.lim = tttime) {
    require(xts)
    ts.num <- length(time.series)
    ## Read time-series
    for(act.ts.num in 1:ts.num) {
        ts.readed.window <- get(time.series[act.ts.num])[time.lim]
        assign(paste("ts", act.ts.num, sep="."), ts.readed.window)
    }
    ## Plot time-series
    plot.zoo(zoo(cumsum(coredata(Boreas.xts[time.lim])),time(Boreas.xts[time.lim])), xlab="",ylab="Precipitation [mm]", type = "n")
    grid(nx = NA, ny = NULL)
    for(act.ts.num in 1:ts.num) {
        ts.to.plot <- get(paste("ts", act.ts.num, sep="."))
        lines(zoo(cumsum(coredata(ts.to.plot)),time(ts.to.plot)),lwd=2, col=act.ts.num)
    }
    legend("bottomright", legend=c("Samp.: 10 min, Res.: 0.1 mm", "Samp.: 1 min, Res.: 0.1 mm", "Samp.: 1 sec, Res.: 0.5 mm"), lwd = 2, col=c(1,2,4))
}

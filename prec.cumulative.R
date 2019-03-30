prec.cumulative <- function(time.series=c("Boreas.xts", "c1.xts", "hhm.xts"), time.lim = tttime) {
    require(xts)
    ts.num <- length(time.series)
    ts.end.values <- numeric()
    ## Read time-series and pre-process
    for(act.ts.num in 1:ts.num) {
        ## Read in and cut
        ts.readed.window <- get(time.series[act.ts.num])[time.lim]
        ## Cumulate coredata
        ts.cumulated.coredata <- cumsum(coredata(ts.readed.window))
        ## Collect end value
        ts.length <- length(ts.cumulated.coredata)
        ts.end.values <- c(ts.end.values, ts.cumulated.coredata[ts.length])
        ## Generate new zoo series
        ts.cumulated <- zoo(ts.cumulated.coredata, time(ts.readed.window))
        assign(paste("ts", act.ts.num, sep="."), ts.cumulated)
    }

    ## Select highest value for y-axis limit
    maximal.prec <- max(ts.end.values)

    ## Plot time-series
    plot(ts.1, xlab="",ylab="Precipitation [mm]", type = "n",
         ylim = c(0, maximal.prec),
         main = time.lim
         )
    grid(nx = NA, ny = NULL)
    for(act.ts.num in 1:ts.num) {
        ts.to.plot <- get(paste("ts", act.ts.num, sep="."))
        lines(ts.to.plot, lwd=2, col=act.ts.num)
    }
    legend("bottomright", legend=c("Samp.: 10 min, Res.: 0.1 mm", "Samp.: 1 min, Res.: 0.1 mm", "Samp.: 1 sec, Res.: 0.5 mm"), lwd = 2, col=c(1,2,4))
}

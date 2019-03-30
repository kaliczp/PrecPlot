prec.cumulative <- function(time.series=c("Boreas.xts", "c1.xts", "hhm.xts"), time.lim = tttime) {
    require(xts)
    ## Read time-series
    ts.1 <- get(time.series[1])
    ts.2 <- get(time.series[2])
    ts.3 <- get(time.series[3])
    ## Plot time-series
    plot.zoo(zoo(cumsum(coredata(Boreas.xts[time.lim])),time(Boreas.xts[time.lim])), xlab="",ylab="Precipitation [mm]", type = "n")
    grid(nx = NA, ny = NULL)
    lines(zoo(cumsum(coredata(ts.1[time.lim])),time(ts.1[time.lim])),lwd=2, col=1)
    lines(zoo(cumsum(coredata(ts.2[time.lim])),time(ts.2[time.lim])), lwd=2, col=2)
    lines(zoo(cumsum(coredata(ts.3[time.lim])),time(ts.3[time.lim])), lwd=2, col=4)
    legend("bottomright", legend=c("Samp.: 10 min, Res.: 0.1 mm", "Samp.: 1 min, Res.: 0.1 mm", "Samp.: 1 sec, Res.: 0.5 mm"), lwd = 2, col=c(1,2,4))
}

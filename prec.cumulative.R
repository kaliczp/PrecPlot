prec.cumulative <- function(time.lim = tttime) {
    require(xts)
    plot.zoo(zoo(cumsum(coredata(Boreas.xts[time.lim])),time(Boreas.xts[time.lim])), xlab="",ylab="Precipitation [mm]", type = "n")
    grid(nx = NA, ny = NULL)
    lines(zoo(cumsum(coredata(Boreas.xts[time.lim])),time(Boreas.xts[time.lim])),lwd=2, col=1)
    lines(zoo(cumsum(coredata(c1.xts[time.lim])),time(c1.xts[time.lim])), lwd=2, col=2)
    lines(zoo(cumsum(coredata(hhm.xts[time.lim])),time(hhm.xts[time.lim])), lwd=2, col=4)
    legend("bottomright", legend=c("Samp.: 10 min, Res.: 0.1 mm", "Samp.: 1 min, Res.: 0.1 mm", "Samp.: 1 sec, Res.: 0.5 mm"), lwd = 2, col=c(1,2,4))
}

prec.cumulative <- function(time.series=c("Boreas.xts", "c1.xts", "hhm.xts"), time.lim = tttime, points = FALSE) {
    require(xts)

    ## Start and end time in POSIXct
    if(is.character(time.lim)) {
        times.splitted <- unlist(strsplit(time.lim, split = "/"))
        start.time <- as.POSIXct(times.splitted[1])
        end.time <- as.POSIXct(times.splitted[2])
    } else {
        if(is.timeBased(time.lim[1])) {
            start.time <- time.lim[1]
            end.time <- time.lim[2]
            time.lim <- paste0(strftime(time.lim[1], format = "%Y-%m-%d %H:%M"),
                               "/",
                               strftime(time.lim[2], format = "%Y-%m-%d %H:%M")
                               )
        }
    }
    ts.num <- length(time.series)
    ts.end.values <- numeric()
    ## Read time-series and pre-process
    for(act.ts.num in 1:ts.num) {
        ## Read in and cut
        ts.readed.window <- get(time.series[act.ts.num])[time.lim]
        ## Extract time stamps
        ts.timestamps <- time(ts.readed.window)
        ## Extract coredata
        ts.coredata <- coredata(ts.readed.window)
        ## Put zero precipitation the end and beginning of the valid
        ## time-series if necessary
        if(length(ts.timestamps) > 0) {
            if(ts.timestamps[1] > start.time) {
                ts.timestamps <- c(start.time, ts.timestamps)
                ts.coredata <- c(0, ts.coredata)
            }
            if(ts.timestamps[length(ts.timestamps)] < end.time) {
                ts.timestamps <- c(ts.timestamps, end.time)
                ts.coredata <- c(ts.coredata, 0)
            }
        }
        ## Cumulate coredata
        ts.cumulated.coredata <- cumsum(ts.coredata)
        ## Collect end value
        ts.length <- length(ts.cumulated.coredata)
        ts.end.values <- c(ts.end.values, ts.cumulated.coredata[ts.length])
        ## Generate new zoo series
        ts.cumulated <- zoo(ts.cumulated.coredata, ts.timestamps)
        assign(paste("ts", act.ts.num, sep="."), ts.cumulated)
    }

    ## Select highest value for y-axis limit
    maximal.prec <- max(ts.end.values, na.rm = TRUE)

    ## Plot time-series
    plot(ts.1, xlab="",ylab="Precipitation [mm]", type = "n",
         ylim = c(0, maximal.prec),
         main = time.lim
         )
    grid(nx = NA, ny = NULL)
    for(act.ts.num in 1:ts.num) {
        ts.to.plot <- get(paste("ts", act.ts.num, sep="."))
        lines(ts.to.plot, lwd=2, col=act.ts.num)
        if(points) {
            points(ts.to.plot, col=act.ts.num)
        }
    }
    legend("bottomright", legend=time.series, lwd = 2, col=1:length(time.series))
}

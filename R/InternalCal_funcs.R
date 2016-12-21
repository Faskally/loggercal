#' Add together two numbers.
#'
#' Description
#'
#' @param fname A number.
#' @return The sum of \code{x} and \code{y}.
#' @export

readInternalCal <- function(fname) {
  crosscal <- read.csv(fname, skip = 5, header = FALSE, row.names = 1)

  # only use complete cases... so if a mix of loggers used this could cause
  # problems...
#  crosscal <- subset(crosscal, complete.cases(crosscal))

  meta <- read.csv(fname, skip = 1, nrows = 4, header = FALSE, row.names = 1, stringsAsFactors = FALSE)[-1]
  meta <- as.data.frame(t(meta), stringsAsFactors = FALSE)
  rownames(meta) <- NULL
  names(meta) <- gsub("/", "", names(meta))
  #meta $ ukas <- meta $ SN %in% calibrationSN # the calibration loggers: c("319151", "613892")

  time <- crosscal[[1]]
  date <- data.frame(date = strptime(time, format = "%d/%m/%Y %H:%M"))

  #day1 <- paste(substring(paste(date $ date[1]), 1, 10), "00:00:00")
  #day1 <- strptime(day1, format = "%Y-%m-%d %H:%M:%S")
  day1 <- date $ date[1]
  date $ sec <- as.numeric(date $ date) - as.numeric(day1)

  temperatures <- as.matrix(crosscal[-1])
  colnames(temperatures) <- meta $ SN

  out <- list(data = temperatures,
              date = date,
              meta = meta,
              startStopSec = range(date $ sec))
  class(out) <- "InternalCal"
  out
}





#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param ... Numeric, complex, or logical vectors.
#' @return The sum of \code{x} and \code{y}.
#' @export

print.InternalCal <- function(x, ...) {
  cat("\nInternal calibration experiment:\n")
  # date
  # number of loggers
  # number of total observations (nrow(data))
  # number of concurent observations
  # calibration logger??
  print(x $ meta)
}





#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param type A number.
#' @param xlim A number.
#' @param main A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom grDevices grey
#' @importFrom graphics abline
#' @importFrom graphics matplot

plot.InternalCal <- function(x, type = "scaled", xlim = NULL, main = "") {
  type <- match.arg(type, c("scaled", "raw"))

  xval <- x $ date $ sec / 60

  if (type == "scaled") {
    yval <- sweep(x $ data, 1, rowMeans(x $ data, na.rm = TRUE), "-")
  } else {
    yval <- x $ data
  }

  # calculate y limits
  which <- x $ date $ sec > (x $ startStopSec[1]) &
           x $ date $ sec < (x $ startStopSec[2])
  if (!is.null(xlim)) {
    which <- which &
             x $ date $ sec > (xlim[1] * 60) &
             x $ date $ sec < (xlim[2] * 60)
  }

  ylim <- range(yval[which,], na.rm = TRUE)

  matplot(y = yval, x = xval, type = "l", lty = 1,
          ylab = expression(paste("Temperature ", degree,"C", sep = "")),
          xlab = "Minutes since start of experiment",
          ylim = ylim, xlim = xlim, main = main)
  abline(v = x $ startStopSec / 60, col = grey(0.5), lty = 2)
}

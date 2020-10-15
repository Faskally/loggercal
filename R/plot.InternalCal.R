#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param type A number.
#' @param xlim A number.
#' @param ... other graphics parameters to pass on to plotting commands.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom grDevices grey
#' @importFrom graphics abline
#' @importFrom graphics matplot

plot.InternalCal <- function(x, type = "scaled", xlim = NULL, ...) {
  type <- match.arg(type, c("scaled", "raw"))

  xval <- x$ date$ sec / 60

  if (type == "scaled") {
    yval <- sweep(x$ data, 1, rowMeans(x$ data, na.rm = TRUE), "-")
  } else {
    yval <- x$ data
  }

  # calculate x limits
  which <- x$ date$ sec > (x$ startStopSec[1]) &
    x$ date$ sec < (x$ startStopSec[2])
  if (!is.null(xlim)) {
    which <- which &
      x$ date$ sec > (xlim[1] * 60) &
      x$ date$ sec < (xlim[2] * 60)
  }

  ylim <- range(yval[which, ], na.rm = TRUE)

  matplot(
    y = yval, x = xval, type = "l", lty = 1,
    ylab = expression(paste("Temperature ", degree, "C", sep = "")),
    xlab = "Minutes since start of experiment",
    ylim = ylim, xlim = xlim, ...
  )
  abline(v = x$ startStopSec / 60, col = grey(0.5), lty = 2)
}

#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param ... Numeric, complex, or logical vectors.
#' @return The sum of \code{x} and \code{y}.
#' @export
trim <- function(x, ...) {
  which <- x$date$sec > (x$startStopSec[1]) &
    x$date$sec < (x$startStopSec[2])

  x$data <- x$ data[which, ]
  x$date <- x$ date[which, ]
  x$date$sec <- x$date$sec - min(x$date$sec) + 1
  x$startStopSec <- c(0, max(x$date$sec) + 1)

  x
}

#' Add together two numbers.
#'
#' Description
#'
#' @param cal A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom utils tail
findStopSec <- function(cal) {
  main <- "Zoom and choose an end point\nClick stop when done"
  plot(cal, main = main, type = "raw")

  loc2 <- locator(n = 1)
  while (!is.null(loc2)) {
    cal$ startStopSec[2] <- loc2$ x * 60
    endSec <- tail(cal$ date$ sec, 1)
    xlim1 <- (endSec - (endSec - loc2$ x * 60) * 2) / 60
    plot(cal, xlim = c(xlim1, endSec / 60), main = main, type = "raw")
    loc2 <- locator(n = 1)
  }
  cal
}

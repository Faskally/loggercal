#' Add together two numbers.
#'
#' Description
#'
#' @param cal A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom graphics plot
#' @importFrom graphics locator
findStartSec <- function(cal) {
  main <- "Zoom and choose an start point\nClick stop when done"
  plot(cal, main = main, type = "raw")

  loc1 <- locator(n = 1)
  while (!is.null(loc1)) {
    cal$ startStopSec[1] <- loc1$ x * 60
    plot(cal, xlim = c(0, loc1$ x * 2), main = main, type = "raw")
    loc1 <- locator(n = 1)
  }
  cal
}

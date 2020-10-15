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
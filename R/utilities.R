#' @export
`[.InternalCal` <- function(x, i, ...) {
  out <-
    list(
      data = x$data[, i, drop = FALSE],
      date = x$date,
      meta = x$meta[i,],
      startStopSec = x$startStopSec
    )
  class(out) <- class(x)
  # return
  out
}

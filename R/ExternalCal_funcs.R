#' Add together two numbers.
#'
#' Description
#'
#' @param fnames A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom utils read.csv
readExternalCal <- function(fnames) {
  data <- lapply(fnames, function(x) {
    out <- read.csv(fnames[1], skip = 1)
    names(out)[1:2] <- c("control", "cal")
    out
  })

  SN <- sapply(fnames, function(x) paste(read.csv(x, nrows = 1, header = FALSE)[[2]]))
  SN <- unname(SN)

  names(data) <- SN

  out <- list(SN = SN, data = data)
  class(out) <- "ExternalCal"
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
print.ExternalCal <- function(x, ...) {
  cat("\nExternal calibration serial numbers:\n")
  # date
  # number of loggers
  # number of total observations (nrow(data))
  # number of concurent observations
  # calibration logger??
  print(x $ SN)
}

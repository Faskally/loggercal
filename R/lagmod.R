#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param y A number.
#' @param lag A number.
#' @param formula A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom utils head
#' @importFrom mgcv gam
lagmod <- function(x, y, lag = 0, formula = y ~ s(x, k = 3)) {
  if (lag > 0) {
    x <- head(x, length(x) - lag)
    y <- tail(y, length(y) - lag)
  } else {
    x <- tail(x, length(x) + lag)
    y <- head(y, length(y) + lag)
  }
  gam(formula, data = list(x = x, y = y))
}

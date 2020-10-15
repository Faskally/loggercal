#' Add together two numbers.
#'
#' Description
#'
#' @param internalCal A number.
#' @param lagtry A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom stats dnorm
#' @importFrom stats sd
findLag <- function(internalCal, lagtry = -10:10) {

  resids <- function(lag = 0) {
    apply(internalCal $ data[,!internalCal $ meta $ ukas, drop = FALSE], 2,
      function(x, y, lag) residuals(lagmod(x, y, lag)),
      y = internalCal $ data[,internalCal $ meta $ ukas, drop = FALSE][,1],
      lag = lag)
  }

  lagLik <- function(lag) {
    r <- resids(lag)
    sig <- sd(c(r))
    sum(dnorm(r, 0, sig, log = TRUE))
  }

  #TODO generalise lagtry this for different sampling schemes
  ll <- sapply(lagtry, lagLik)

  lagtry[which.max(ll)]
}

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
  while(!is.null(loc1)) {
    cal $ startStopSec[1] <- loc1 $ x * 60
    plot(cal, xlim = c(0, loc1 $ x * 2), main = main, type = "raw")
    loc1 <- locator(n = 1)
  }
  cal
}





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
  while(!is.null(loc2)) {
    cal $ startStopSec[2] <- loc2 $ x * 60
    endSec <- tail(cal $ date $ sec,1)
    xlim1 <- (endSec - (endSec - loc2 $ x*60) * 2) / 60
    plot(cal, xlim = c(xlim1, endSec/60), main = main, type = "raw")
    loc2 <- locator(n = 1)
  }
  cal
}




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




#' Add together two numbers.
#'
#' Description
#'
#' @param x A number.
#' @param ... Numeric, complex, or logical vectors.
#' @return The sum of \code{x} and \code{y}.
#' @export
trim <- function(x, ...) {
  which <- x $ date $ sec > (x $ startStopSec[1]) &
           x $ date $ sec < (x $ startStopSec[2])

  x $ data <- x $ data[which,]
  x $ date <- x $ date[which,]
  x $ date $ sec <- x $ date $ sec - min(x $ date $ sec) + 1
  x $ startStopSec <- c(0, max(x $ date $ sec) + 1)

  x
}


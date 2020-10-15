#' Add together two numbers.
#'
#' Description
#'
#' @param reverseCalMod A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
#' @importFrom stats coef
#' @importFrom stats lm
getCoefs <- function(reverseCalMod) {
  Cal_coef <-
    sapply(reverseCalMod, function(mods) {
      x <- seq(0, 30, length = 100)
      Cal_coef1 <- rep(1, length(x))
      Cal_coef2 <- x
      Cal_coef3 <- x^2
      y <- predict(mods[["CalMod"]], newdata = data.frame(targets = x))
      coef(lm(y ~ Cal_coef1 + Cal_coef2 + Cal_coef3 - 1))
    })

  SE_coef <-
    sapply(reverseCalMod, function(mods) {
      x <- seq(0, 30, length = 100)
      SE_coef1 <- rep(1, length(x))
      SE_coef2 <- x
      SE_coef3 <- x^2
      SE_coef4 <- x^3
      SE_coef5 <- x^4
      y <- predict(mods[["CalSEMod"]], newdata = data.frame(targets = x))
      coef(lm(y ~ SE_coef1 + SE_coef2 + SE_coef3 + SE_coef4 + SE_coef5 - 1))
    })

  list(Cal_coef = Cal_coef, SE_coef = SE_coef)
}

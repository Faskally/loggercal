#' Fit a calibration regression and estimate logger error.
#'
#' This function:
#'    * identifies calibration loggers
#'    * chooses one calibration logger to be used
#'    * finds the best lag
#'    * fits a calibration model to each non-calibration logger
#'    * returns models for calibration and SE models
#'
#' @param internalCal A number.
#' @param externalCalMod A number.
#' @param n A number.
#'
#' @return models for calibration and SE models.
#'
#' @importFrom stats predict
#' @importFrom stats residuals
#' @importFrom stats rnorm
#'
#' @export

calibration <- function(internalCal, externalCalMod, n = 99) {

  message("Setting up calibration regressions.")

  # define which loggers are calibration loggers
  calibrationSN <- names(externalCalMod)
  internalCal$meta$ukas <- internalCal$meta$SN %in% calibrationSN
  calID <- which(internalCal$meta$ukas)

  if (length(calID) == 0)
    stop("Calibration loggers do not match any logger used in experiment.")
  if (length(calID) > 1)
    message("multiple caliibration loggers found in experiment. Only the first used.")
  calSN <- internalCal$meta$SN[calID[1]]
  internalCal <- trim(internalCal)

  lag <- findLag(internalCal)

  internalCalMod <-
    apply(internalCal$data[,!internalCal$meta$ukas], 2,
          lagmod,
          y = internalCal$data[,internalCal$meta$ukas, drop = FALSE][,1],
          lag = lag)

  rtrue2test <- function(true, ukasmodel, calmodel, n = 1000) {
    predcal <- predict(ukasmodel, newdata = data.frame(control = true), se = TRUE)
    rpredcal <- rnorm(n, predcal $ fit, predcal $ se.fit + summary(ukasmodel) $ sigma)
    predtest <- predict(calmodel, newdata = list(x = rpredcal), se = TRUE)
    rnorm(n, predtest $ fit, predtest $ se.fit + summary(calmodel) $ sigma)
  }

  out <-
    lapply(names(internalCalMod),
      function(x) {
        ith <- which(names(internalCalMod) == x)
        cat("fitting ", x, ": ", ith, " of ", length(internalCalMod), "\n", sep = "")
        loggermod <- internalCalMod[[x]]
        # select the appropriate external calibration model
        calibrationmod <- externalCalMod[[calSN]]
        # choose 'design' points to predict on
        targets <- 0:30
        reverse <-
          lapply(targets, function(trgt) {
            # what is a sensible range to look for the truth over...
            trues <- trgt + seq(-0.3, 0.3, by = 0.001)
            probs <- sapply(trues, function(i) {
                ts <- rtrue2test(i, calibrationmod, loggermod, n = n)
                mean(abs(ts - trgt) < 0.001)
              })
            probs <- probs / sum(probs)
            list(x = trues, prob = probs)
      })

      # simulatereverseplots
      loggermeans <- sapply(reverse, function(x) sum(x $ x * x $ prob))
      CalMod <- lm(loggermeans ~ poly(targets, 2))

      loggerse <- sapply(reverse, function(x) with(x, sqrt(sum((x - sum(x * prob))^2 * prob))))
      CalSEMod <- lm(loggerse ~ poly(targets, 4))

      list(CalMod = CalMod, CalSEMod = CalSEMod)
    })

  names(out) <- names(internalCalMod)

  out
}


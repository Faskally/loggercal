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
#' @importFrom stats fitted
#' @importFrom stats optim
#' @importFrom stats sigma
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

#  lag <- findLag(internalCal)

  internalCalMod <-
    apply(internalCal$data[,!internalCal$meta$ukas, drop = FALSE],
          2,
          lagmod,
          x = internalCal$data[,calSN],
          lag = 0,
          formula = y ~ s(x, k=5))

  # ukasmodel <- externalCalMod[[1]]
  # calmodel <- internalCalMod[[1]]
  # true <- seq(-0.1, 0.1, length = 100)
  dtrgGivenTrue <- function(true, trgt, ukasmodel, calmodel, log = TRUE) {
    # for a given trueth, what is the distribution of ukas logger temps
    predcal <- predict(ukasmodel, newdata = data.frame(control = true), se = TRUE)
    # for a given ukas logger temp, what is the distribution of the test logger observations
    predtest <- predict(calmodel, newdata = list(x = predcal$fit), se = TRUE)
    #
    dnorm(trgt,
          predtest$fit,
          sqrt(predcal $se.fit^2 + sigma(ukasmodel)^2 +
               predtest$se.fit^2 + sigma(calmodel)^2),
          log = log)
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
        targets <- seq(min(fitted(calibrationmod)),
                       max(fitted(calibrationmod)),
                       length = 40)
        loggerdist <-
          sapply(targets, function(trgt) {
            # maximise the profile likelihood
            f <- function(true) {
              -1 * dtrgGivenTrue(true,
                                 trgt = trgt,
                                 ukasmodel = calibrationmod,
                                 calmodel = loggermod, log = TRUE)
            }
            opt <- suppressWarnings(optim(trgt, f, hessian = TRUE))
            c(mean = opt$par, sd = 1 / sqrt(opt$hessian))
            #if (FALSE) {
            #  # integration points
            #  true <- opt$par + 4 / sqrt(opt$hessian)*seq( -1, 1, length = 1000)
            #  # calculate the exact distribution
            #  probs <- dtrgGivenTrue(true,
            #                       trgt = trgt,
            #                       ukasmodel = calibrationmod,
            #                       calmodel = loggermod, log = FALSE)
            #  probs <- probs / sum(probs)
            #  plot(true, probs, type = "l")
            #  within(list(), {
            #    mean = sum(true * probs)
            #    sd = sqrt(sum((true - mean)^2 * probs))
            #    })
            #}
      })

      loggermeans <- loggerdist["mean",]
      loggerse <- loggerdist["sd",]

      # fit lines to predictions
      CalMod <- lm(loggermeans ~ 1 + poly(targets, 2, raw = FALSE))
      CalSEMod <- lm(loggerse ~ 1 + poly(targets, 4, raw = FALSE))

      list(CalMod = CalMod, CalSEMod = CalSEMod)
    })

  names(out) <- names(internalCalMod)

  out
}


#  Script name:   allFunctions.R
#  Version:       0.0.1
#  Created on:    15/09/2014
#  Author:        Colin Millar, millarc@marlab.ac.uk
#  Purpose:       Contains all functions required to 
#                 (1) Read in temperature logger calibration data 
#                     (External and Internal)
#                 (2) calculate approximate calibration functions
#                     to convert raw temps to true and raw temps 
#                     to SE of approx
#                 (3) output a file containing calibration results by 
#                     logger for use as a bulk upload to FLEObs database
#  History:       0.0.1 Colin Millar 15/09/2014
#                   Created script


doCalibration <- function(nsim = 9999) {

  # 
  # external calibration
  # 
  
  # note file name is: UKASCalibration_[SN].csv
  #externalCal <- choose.files(filters = myFilters, index = 1, caption = "Choose External Calibration files")
  
  # automatically select external calibration files  
  externalCal <- list.files("External Calibrations/", full.names = TRUE)

  # read in external calibration
  ukas <- readExternalCal(externalCal)

  # fit a model to the external calibration data
  externalCalMod <- lapply(ukas $ data, function(x) lm(cal ~ poly(control, 2), data = x[-1,]))
  

  # summarise external calibration being used
  cat("External Calibration data read from:",
    "\n\t", getwd(), "/External Calibrations/\n\n", sep = ""); flush.console()

  #
  # internal calibration
  # 

  cat("Select calibration directory ... "); flush.console()

  # get internal calibration directory
  internalCalDirs <- dir(getwd(), full.names = TRUE)[grep("^[0-9][0-9][0-9][0-9][0-9][0-9]$", dir())]
  # could try and select the newest by default...
  internalCalDir <- choose.dir(default = internalCalDirs[1], caption = "Choose Internal Calibration Directory")
  if (is.na(internalCalDir)) stop("You must choose a directory to continue.")

  if (file.exists(paste0(internalCalDir, "/TimeSeries_RawData.png"))) {
    ans <- winDialog("yesno", message = "It looks like this experiment has already been processed.\n\nDo you want to continue?")
    if (ans == "NO") {
      cat("Stopped\n\n")
      return(invisible())
    }
  }

  cat("Done:",
      "\n\t", internalCalDir, "\n",
      "Reading calibration data ... ", sep = "")
  flush.console()
  
  # read internal calibration experiment file
  internalCalFile <- list.files(internalCalDir)
  which <- grep("^FullCalibration_[0-9][0-9][0-9][0-9][0-9][0-9].csv$", internalCalFile)
  internalCalFile <- list.files(internalCalDir, full.names = TRUE)[which]

  # read in internal calibration experiment
  internalCal <- readInternalCal(internalCalFile)

  cat("Done\n",
      "Select start and stop points ... ", sep = "")
  flush.console()

  # trim off ends
  internalCal <- findStartSec(internalCal)
  internalCal <- findStopSec(internalCal)
  #plot(InternalCal)
  # do we want to check this is okay?
  # we can simply loop the last bit and we can reselect or not new start points
  dev.off()

  cat("Done\n",
      "Saving plots of calibration data ... ", sep = ""); flush.console()

  #
  # save some diagnostic plots
  #

  savePlot(paste0(internalCalDir, "/", "TimeSeries_MeanCorrected.png"), 
           function() {
            plot(internalCal)
          })

  savePlot(paste0(internalCalDir, "/", "TimeSeries_RawData.png"), 
           function() {
            plot(internalCal, type = "raw")
           })

  cat("Done\n",
      "Running calibration (could take some time!) ... ", sep = "")
  flush.console()

  #
  # now to perform a calibration
  # 
   
  cal <- calibration(internalCal, externalCalMod, n = nsim)

  cat("Done\n",
      "Saving table of coefficients ... ", sep = "")
  flush.console()

  # convert coefficients to non orthogonal polynomials
  coefs <- getCoefs(cal)
  
  tabCoefs <- tableCoefs(coefs, cal = internalCal)

  write.csv(file = paste0(internalCalDir, "\\coefficients.csv"), tabCoefs, row.names = FALSE)
  
  cat("Done\n",
    "\nCalibration complete!\n",
        "Coefficient file and diagnostic plots in folder:\n",
        "\t", internalCalDir, "\n\n", sep = "")
}



savePlot <- function(filename, plotFunc, width = 17, height = 17, ppi = 1200) {
  png(file = filename, width / 2.54 * ppi, height / 2.54 * ppi, res = ppi)
  plotFunc()
  dev.off()
}



# FILTERS FOR CHOOSEING CALIBRATION FILES.
# note only available on windows.
myFilters <- matrix(c("UKAS calibration", "UKAS*.csv",
                      "Internal calibration", "full*.csv",
                      "all calibration", "*.csv",
                      "all files", "*.*"), ncol = 2, byrow = TRUE)


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



lagmod <- function(x, y, lag = 0, formula = y ~ poly(x, 2)) {
  if (lag > 0) {
    x <- head(x, length(x) - lag)
    y <- tail(y, length(y) - lag)
  } else {
    x <- tail(x, length(x) + lag)
    y <- head(y, length(y) + lag)    
  }
  lm(formula)
}


# get lag
findLag <- function(cal, lagtry = -10:10) {

  resids <- function(lag = 0) {
    apply(cal $ data[,!cal $ meta $ ukas], 2, 
      function(x, y, lag) residuals(lagmod(x, y, lag)), 
      y = cal $ data[,cal $ meta $ ukas][,1], 
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



readInternalCal <- function(fname) {
  crosscal <- read.csv(fname, skip = 5, header = FALSE, row.names = 1)
  
  # only use complete cases... so if a mix of loggers used this could cause
  # problems...
#  crosscal <- subset(crosscal, complete.cases(crosscal))
  
  meta <- read.csv(fname, skip = 1, nrows = 4, header = FALSE, row.names = 1, stringsAsFactors = FALSE)[-1]
  meta <- as.data.frame(t(meta), stringsAsFactors = FALSE)
  rownames(meta) <- NULL
  names(meta) <- gsub("/", "", names(meta))
  #meta $ ukas <- meta $ SN %in% calibrationSN # the calibration loggers: c("319151", "613892")

  time <- crosscal[[1]]
  date <- data.frame(date = strptime(time, format = "%d/%m/%Y %H:%M"))

  #day1 <- paste(substring(paste(date $ date[1]), 1, 10), "00:00:00")
  #day1 <- strptime(day1, format = "%Y-%m-%d %H:%M:%S")
  day1 <- date $ date[1]
  date $ sec <- as.numeric(date $ date) - as.numeric(day1)

  temperatures <- as.matrix(crosscal[-1])
  colnames(temperatures) <- meta $ SN

  out <- list(data = temperatures, 
              date = date, 
              meta = meta, 
              startStopSec = range(date $ sec))  
  class(out) <- "InternalCal"
  out
}

print.InternalCal <- function(x, ...) {
  cat("\nInternal calibration experiment:\n")
  # date
  # number of loggers
  # number of total observations (nrow(data))
  # number of concurent observations
  # calibration logger??
  print(x $ meta)
}

plot.InternalCal <- function(x, type = "scaled", xlim = NULL, main = "") {
  type <- match.arg(type, c("scaled", "raw"))

  xval <- x $ date $ sec / 60

  if (type == "scaled") {
    yval <- sweep(x $ data, 1, rowMeans(x $ data, na.rm = TRUE), "-")
  } else {
    yval <- x $ data
  }

  # calculate y limits
  which <- x $ date $ sec > (x $ startStopSec[1]) &
           x $ date $ sec < (x $ startStopSec[2])
  if (!is.null(xlim)) {
    which <- which &
             x $ date $ sec > (xlim[1] * 60) &
             x $ date $ sec < (xlim[2] * 60)
  }

  ylim <- range(yval[which,], na.rm = TRUE)

  matplot(y = yval, x = xval, type = "l", lty = 1,
          ylab = expression(paste("Temperature ", degree,"C", sep = "")),
          xlab = "Minutes since start of experiment",
          ylim = ylim, xlim = xlim, main = main)
  abline(v = x $ startStopSec / 60, col = grey(0.5), lty = 2)
}




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

print.ExternalCal <- function(x, ...) {
  cat("\nExternal calibration serial numbers:\n")
  # date
  # number of loggers
  # number of total observations (nrow(data))
  # number of concurent observations
  # calibration logger??
  print(x $ SN)
}

trim <- function(x, ...) {
  which <- x $ date $ sec > (x $ startStopSec[1]) &
           x $ date $ sec < (x $ startStopSec[2])
  
  x $ data <- x $ data[which,]
  x $ date <- x $ date[which,]
  x $ date $ sec <- x $ date $ sec - min(x $ date $ sec) + 1
  x $ startStopSec <- c(0, max(x $ date $ sec) + 1)

  x
}



calibration <- function(internalCal, externalCalMod, n = 99) {
# this function:
# - identifies calibration loggers
# - chooses one calibration logger to be used
# - finds the best lag
# - fits a calibration model to each non-calibration logger
# - returns models for calibration and SE models

  message("Setting up calibration regressions.")

  # define which loggers are calibration loggers
  calibrationSN <- names(externalCalMod)
  internalCal $ meta $ ukas <- internalCal $ meta $ SN %in% calibrationSN
  calID <- which(internalCal $ meta $ ukas)
  if (length(calID) == 0) 
    stop("Calibration loggers do not match any logger used in experiment.")
  if (length(calID) > 1) 
    message("multiple caliibration loggers found in experiment. Only the first used.")
  calSN <- internalCal $ meta $ SN[calID[1]]
  internalCal <- trim(internalCal)

  lag <- findLag(internalCal)

  internalCalMod <- 
    apply(internalCal $ data[,!internalCal $ meta $ ukas], 2, 
          lagmod, 
          y = internalCal $ data[,internalCal $ meta $ ukas][,1], 
          lag = lag)

  rtrue2test <- function(true, ukasmodel, calmodel, n = 1000) {
    predcal <- predict(ukasmodel, newdata = data.frame(control = true), se = TRUE)
    rpredcal <- rnorm(n, predcal $ fit, predcal $ se.fit + summary(ukasmodel) $ sigma)
    predtest <- predict(calmodel, newdata = list(x = rpredcal), se = TRUE)
    rnorm(n, predtest $ fit, predtest $ se.fit + summary(calmodel) $ sigma)
  }

  out <- 
    lapply(names(internalCalMod), function(x) {
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


      #```{r simulatereverseplots}
      loggermeans <- sapply(reverse, function(x) sum(x $ x * x $ prob))
      CalMod <- lm(loggermeans ~ poly(targets, 2))

      loggerse <- sapply(reverse, function(x) with(x, sqrt(sum((x - sum(x * prob))^2 * prob))))
      CalSEMod <- lm(loggerse ~ poly(targets, 4))

      list(CalMod = CalMod, CalSEMod = CalSEMod)
    })

  names(out) <- names(internalCalMod)

  out
}

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


tableCoefs <- function(coefs, cal) {

  whichsn <- colnames(coefs $ Cal_coef)
  meta <- cal $ meta
  rownames(meta) <- meta $ SN
  meta <- meta[whichsn,]

  bulkout <- 
    data.frame(Make = meta $ Description,
               Model = meta $ Type,
               SN = meta $ SN,
               Unit = "\260C", # degrees C
               Start_date = format(cal $ date $ date[1], format = "%Y-%m-%d"), # strip off minutes
               Cal_Eq_type = 2,
               Cal_coef1 = "", 
               Cal_coef2 = "",
               Cal_coef3 = "",
               Cal_coef4 = "",
               Cal_coef5 = "",
               Cal_coef6 = "",
               Cal_coef7 = "",
               Cal_coef8 = "",
               Cal_coef9 = "",
               Cal_coef10 = "",
               SE_Eq_type = 4,
               SE_coef1 = "",
               SE_coef2 = "",
               SE_coef3 = "",
               SE_coef4 = "",
               SE_coef5 = "",
               SE_coef6 = "",
               SE_coef7 = "",
               SE_coef8 = "",
               SE_coef9 = "",
               SE_coef10 = "")

  Cal_coef <- as.data.frame(t(coefs $ Cal_coef))
  Cal_coef[] <- lapply(Cal_coef, paste)
  bulkout[names(Cal_coef)] <- Cal_coef
  SE_coef <- as.data.frame(t(coefs $ SE_coef))
  SE_coef[] <- lapply(SE_coef, paste)
  bulkout[names(SE_coef)] <- SE_coef

  bulkout
}




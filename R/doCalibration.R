
#' POINT AND CLICK CALIBRATION CODE

# Function arguments:

# internalCalDir = the filepath of where the internal calibration file is located

# externalCalDir = the filepath of where the external calibration file is located which
#                  is appropriate for the internal calibration that you are working on

# ukas = this reads the external calibration in

# externalCalMod = this produces a model from the external calibration data

# internalCal = this reads the internal calibration file in

#-------------------------------------------------------------------------------------
# CHOOSE WHERE YOUR FILES ARE LOCATED
#-------------------------------------------------------------------------------------


#' @importFrom grDevices pdf
#' @importFrom graphics lines par
#' @importFrom utils choose.dir flush.console write.csv
#' @export
doCalibration <- function(path = ".", nsim = 9999) {


  # CHOOSE THE FILEPATH WHERE YOUR INTERNAL CALIBRATION EXPERIMENT RESIDES
  internalCalDirs <- dir(path, full.names = TRUE)[grep("^[0-9][0-9][0-9][0-9][0-9][0-9]$", dir())]
  internalCalDir <- choose.dir(default = internalCalDirs[1], caption = "CHOOSE INTERNAL CALIBRATION DIRECTORY")

  # summarise internal calibration being used
  cat("Internal Calibration data read from:",
    "\n\t", internalCalDir,
    sep = ""
  )
  flush.console()

  # CHOOSE THE FILEPATH WHERE YOUR EXTERNAL CALIBRATION RESIDES
  externalCalDirs <- dir(getwd(), full.names = TRUE)[grep("^[0-9][0-9][0-9][0-9][0-9][0-9]$", dir())]
  externalCalDir <- choose.dir(default = externalCalDirs[1], caption = "CHOOSE EXTERNAL CALIBRATION DIRECTORY")

  # summarise external calibration being used
  cat("\n\tExternal Calibration data read from:",
    "\n\t", externalCalDir,
    sep = ""
  )
  flush.console()

  # READ THE EXTERNAL CALIBRATION FILE IN
  cat("\n\tReading external calibration data ... ", "\n\t")
  flush.console()

  ukas <- readExternalCal(paste0(externalCalDir))

  cat("\n\tDONE!", "\n\t")
  flush.console()

  externalCalMod <- lapply(ukas$ data, function(x) mgcv::gam(cal ~ s(control, k = 3), data = x))

  # READ THE INTERNAL CALIBRATION FILE IN
  cat("\n\tReading internal calibration data ... ", "\n\t")
  flush.console()

  internalCal <- readInternalCal(paste0(internalCalDir))

  cat("\n\tDONE!", "\n\t")
  flush.console()

  #----------------------------------------------------
  # SELECT THE START AND STOP TIME FOR THE EXPERIMENT
  #----------------------------------------------------

  internalCal <- findStartSec(internalCal)
  internalCal <- findStopSec(internalCal)

  #------------------------------------------------------------------------------
  #
  # PERFORM A CALIBRATION FOR EACH SERIAL NUMBER WITHIN THE "FullCalibration" CSV
  #
  #------------------------------------------------------------------------------

  cal <- calibration(internalCal, externalCalMod, n = 999)

  #------------------------------
  #
  # EXTRACT THE COEFFICIENTS
  #
  #------------------------------

  cal_coef <- getCoefs(cal)

  #-----------------------------------------------------------------------------------
  #
  # EXPORT SOME PLOTS AS A PDF WHICH WILL BE SAVED IN THE INTERNAL CALIBRATION FOLDER

  #-----------------------------------------------------------------------------------
  # THIS IS THE PLOT CODE CALLED IN LATER
  #-----------------------------------------
  quick_plot <- function(logger_number) {
    tmp <- 0:30
    est <- sapply(tmp, function(x) sum(x^(0:2) * cal_coef$Cal_coef[, logger_number]))
    se <- sapply(tmp, function(x) sum(x^(0:4) * cal_coef$SE_coef[, logger_number]))

    plot(tmp, tmp - est,
      type = "l",
      ylim = range(tmp + 2 * se - est, tmp - 2 * se - est),
      ylab = "temperature bias", xlab = "temperature",
      las = 1, main = logger_number
    )
    lines(tmp, tmp + 2 * se - est, lty = 2)
    lines(tmp, tmp - 2 * se - est, lty = 2)
  }

  #---------------
  # EXPORT PDF
  #---------------
  pdf(paste0(internalCalDir, "/", "full_calibration_plots_", paste0(basename(internalCalDir)), ".pdf"), onefile = TRUE, paper = "a4")

  par(mfrow = c(1, 1))

  # check fit
  temp <- seq(0, 40, length = 100)

  plot1 <- tryCatch(
    {
      if (length(ukas$data[]) == 1) {
        plot(cal - control ~ control,
          data = ukas$data[[1]], pch = 16, cex = 0.5,
          ylab = "calibration logger bias", xlab = "temperature", las = 1
        )
        lines(temp, predict(externalCalMod[[1]], list(control = temp)) - temp)
      } else {
        plot(cal - control ~ control,
          data = ukas$data[[1]], pch = 16, cex = 0.5,
          ylab = "calibration logger bias", xlab = "temperature", las = 1
        )
        lines(temp, predict(externalCalMod[[1]], list(control = temp)) - temp)

        plot(cal - control ~ control,
          data = ukas$data[[2]], pch = 16, cex = 0.5,
          ylab = "calibration logger bias", xlab = "temperature"
        )
        lines(temp, predict(externalCalMod[[2]], list(control = temp)) - temp)
      }
    },
    error = function(e) {
      cat("NO EXTERNAL CALIBRATION LOGGER FOUND", conditionMessage(e), "\n")
    }
  )

  # plot calibration experiment
  plot(internalCal)

  par(mfrow = c(3, 3))
  for (logger_name in names(cal)) {
    quick_plot(logger_name)
  }

  dev.off()

  #---------------------------------------------------------------------
  #
  # SAVE COEFFICIENTS TABLE
  #
  #---------------------------------------------------------------------

  tab <- tableCoefs(cal_coef, internalCal)

  #-----------------------------------------------------------
  #
  # WRITE OUT THE COEFFICIENTS TABLE
  #
  #--------------------------------------------------------------------

  write.csv(
    file = paste0(internalCalDir, "/", "full_calibration_coefs_", paste0(basename(internalCalDir)), ".csv"),
    tab, row.names = FALSE
  )

  cat("Done\n",
    "\nCalibration complete!\n",
    "Coefficient file and diagnostic plots in folder:\n",
    "\t", internalCalDir, "\n\n",
    sep = ""
  )
}

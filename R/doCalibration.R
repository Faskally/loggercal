
#' POINT AND CLICK CALIBRATION CODE
#'
#' Function to provide an easy interface to use the calibation
#' methods in the loggercal package
#'
#' @param path the location on disk where the calibration folders are
#'   initially searched for (the default where the directory selection
#'   dialogue opens).
#' @param nsim number of simulations to run (see \code{\link{calibration}})
#' @param trim if TRUE the user can trim internal logger data at the
#'   start and end of the data series interactively (default = FALSE)
#'
#' @details
#'
#' When running the function the user will be asked to set various
#' directorise where internal and external calibration data are stored:
#' internalCalDir = the filepath of where the internal calibration file is located
#' externalCalDir = the filepath of where the external calibration file is located which
#'                  is appropriate for the internal calibration that you are working on
#'
#' @examples
#'
#' \dontrun{
#' # set up a demo calibration folder structure
#' # please set this to a suitable location on your system
#' path <- "D:\\temp\\Calibration_data"
#' caldir <- file.path(path, "Logger_calibrations", "250416")
#' ukasdir <- file.path(path, "External_Calibrations", "010915")
#' dir.create(caldir, recursive = TRUE)
#' dir.create(ukasdir, recursive = TRUE)
#' file.copy(
#'   system.file(
#'     "calibration_files", "FullCalibration_250416.csv",
#'     package = "loggercal"),
#'   caldir)
#' file.copy(
#'   system.file(
#'     "calibration_files", "UKASCalibration_319151.csv",
#'     package = "loggercal"),
#'   ukasdir)
#'
#'  doCalibration(path, nsim = 10)
#'}
#'
#' @importFrom grDevices pdf
#' @importFrom graphics lines par mtext
#' @importFrom utils flush.console write.csv
#' @importFrom tcltk tk_choose.dir
#' @export
doCalibration <- function(path = ".", nsim = 999, trim = FALSE) {

  choose_directory <- function(default, caption) {
    if (.Platform$OS.type == "windows") {
      utils::choose.dir(default = default, caption = caption)
    } else {
      tcltk::tk_choose.dir(default = default, caption = caption)
    }
  }

  # CHOOSE THE FILEPATH WHERE YOUR INTERNAL CALIBRATION EXPERIMENT RESIDES
  default <- dir(file.path(path, "Logger_calibrations"), full.names = TRUE, pattern = "^[0-9]{6}$")
  if (length(default) == 0) default <- path
  internalCalDir <- choose_directory(default = default, caption = "CHOOSE INTERNAL CALIBRATION DIRECTORY")

  # summarise internal calibration being used
  cat("Internal Calibration data read from:",
    "\n\t", internalCalDir,
    sep = ""
  )
  flush.console()

  # CHOOSE THE FILEPATH WHERE YOUR EXTERNAL CALIBRATION RESIDES
  default <- dir(file.path(path, "External_Calibrations"), full.names = TRUE, pattern = "^[0-9]{6}$")
  if (length(default) == 0) default <- path
  externalCalDir <- choose_directory(default = default, caption = "CHOOSE EXTERNAL CALIBRATION DIRECTORY")

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

  externalCalMod <- lapply(ukas$data, function(x) mgcv::gam(cal ~ s(control, k = 3), data = x))

  # READ THE INTERNAL CALIBRATION FILE IN
  cat("\n\tReading internal calibration data ... ", "\n\t")
  flush.console()

  internalCal <- readInternalCal(paste0(internalCalDir))

  cat("\n\tDONE!", "\n\t")
  flush.console()

  #----------------------------------------------------
  # SELECT THE START AND STOP TIME FOR THE EXPERIMENT
  #----------------------------------------------------

  if (trim) {
    internalCal <- findStartSec(internalCal)
    internalCal <- findStopSec(internalCal)
  }

  #------------------------------------------------------------------------------
  #
  # PERFORM A CALIBRATION FOR EACH SERIAL NUMBER WITHIN THE "FullCalibration" CSV
  #
  #------------------------------------------------------------------------------

  cal <- calibration(internalCal, externalCalMod, nsim = nsim)

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
          ylab = "calibration logger bias", xlab = "temperature", las = 1,
          main = "External logger calibration"
        )
        lines(temp, predict(externalCalMod[[1]], list(control = temp)) - temp)
      } else {
        plot(cal - control ~ control,
          data = ukas$data[[1]], pch = 16, cex = 0.5,
          ylab = "calibration logger bias", xlab = "temperature", las = 1,
          main = "External logger calibration"
        )
        lines(temp, predict(externalCalMod[[1]], list(control = temp)) - temp)

        plot(cal - control ~ control,
          data = ukas$data[[2]], pch = 16, cex = 0.5,
          ylab = "calibration logger bias", xlab = "temperature",
          main = "External logger calibration"
        )
        lines(temp, predict(externalCalMod[[2]], list(control = temp)) - temp)
      }
    },
    error = function(e) {
      cat("NO EXTERNAL CALIBRATION LOGGER FOUND", conditionMessage(e), "\n")
    }
  )

  # plot calibration experiment
  plot(internalCal, main = "Internal logger bias (over time)")

  par(mfrow = c(3, 3))
  for (logger_name in names(cal)) {
    quick_plot(logger_name)
  }
  mtext("Modelled bias for internal loggers", 3, -1, outer = TRUE)

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
    tab, row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  cat("Done\n",
    "\nCalibration complete!\n",
    "Coefficient file and diagnostic plots in folder:\n",
    "\t", internalCalDir, "\n\n",
    sep = ""
  )
}

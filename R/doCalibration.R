#' Run a full calibration
#'
#' This function does everything from read in files, select calibtation period and
#' thens runs a calibration and saves the file.
#'
#' @param nsim The number of simulations to use to evaluate confidence intervals.
#' @return NULL
#' @export
#' @examples
#' 1 + 1
#' @importFrom utils choose.dir
#' @importFrom utils flush.console
#' @importFrom utils winDialog
#' @importFrom utils write.csv

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

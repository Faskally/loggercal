#' Add together two numbers.
#'
#' Description
#'
#' @param dirname A number.
#' @return The sum of \code{x} and \code{y}.
#' @export

readInternalCal <- function(dirname) {

  # form internal calibration file name
  fname <- paste0(dirname, "/FullCalibration_", basename(dirname), ".csv")

  # report files read to user
  message("Reading calibration experiment\n\n",
          "  Current working directory:\n\t", getwd(), "\n",
          "  Files:\n\t",
          paste(path.expand(fname), collapse = "\n\t"))

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

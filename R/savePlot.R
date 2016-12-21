#' Save calibration plot as a png file
#'
#' Description
#'
#' @param filename A number.
#' @param plotFunc A function which returns a plot
#' @param width A number.
#' @param height A number.
#' @param ppi A number.
#' @return NULL
#' @examples
#' 1 + 1
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @export
savePlot <- function(filename, plotFunc, width = 17, height = 17, ppi = 1200) {
  png(filename = filename, width / 2.54 * ppi, height / 2.54 * ppi, res = ppi)
  plotFunc()
  dev.off()
}

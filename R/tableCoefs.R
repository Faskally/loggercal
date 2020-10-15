#' Add together two numbers.
#'
#' Description
#'
#' @param coefs A number.
#' @param cal A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
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
#' Add together two numbers.
#'
#' Description
#'
#' @param coefs A number.
#' @param cal A number.
#' @return The sum of \code{x} and \code{y}.
#' @export
tableCoefs <- function(coefs, cal) {

  whichsn <- colnames(coefs $ Cal_coef)
  meta <- cal $ meta
  rownames(meta) <- meta $ SN
  meta <- meta[whichsn,]

  bulkout <-
    data.frame(
      Make = meta$ Description,
      Model = meta$ Type,
      SN = meta$ SN,
      Unit = "\260C", # degrees C
      Start_date = format(cal$ date$ date[1], format = "%Y-%m-%d"), # strip off minutes
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
      SE_coef10 = ""
    )

  Cal_coef <- as.data.frame(t(coefs $ Cal_coef))
  Cal_coef[] <- lapply(Cal_coef, paste)
  bulkout[names(Cal_coef)] <- Cal_coef
  SE_coef <- as.data.frame(t(coefs $ SE_coef))
  SE_coef[] <- lapply(SE_coef, paste)
  bulkout[names(SE_coef)] <- SE_coef

  bulkout
}

#' The cointmonitoR package
#'
#' Consistent Monitoring of Stationarity and Cointegrating Relationships
#'
#' See the vignette:\cr
#' \code{vignette("cointmonitoR")}
#'
#' See the DESCRIPTION:\cr
#' \code{help(package = cointmonitoR)}
#'
#' See the README:\cr
#' \url{https://github.com/aschersleben/cointmonitoR/blob/master/README.md}
#'
#' Open the package documentation page:\cr
#' \code{package?cointmonitoR}
#'
#' Further information and bug reporting:\cr
#' \url{https://github.com/aschersleben/cointmonitoR}
#'
#' @section Functions:
#' \itemize{
#'   \item \code{\link{monitorCointegration}}\cr
#'         This procedure is able to monitor a cointegration model for level or
#'         trend cointegration and returns the corresponding break point, if
#'         available. It is based on parameter estimation on a pre-break
#'         "calibration" period at the beginning of the sample that is known or
#'         assumed to be free of structural change.
#'
#'   \item \code{\link{monitorStationarity}}\cr
#'         This procedure is a special case of \code{monitorCointegration},
#'         since it's able to monitor a one-dimensional vector for level or
#'         trend stationarity.
#'
#'   \item \code{\link[=print.cointmonitoR]{print}}\cr
#'         Print clear results.
#'   \item \code{\link[=plot.cointmonitoR]{plot}}\cr
#'         Plot the test statitics and the values/residuals of a
#'         \code{cointmonitoR} model.
#' }
#'
#' @section Dependencies:
#' This package mainly depends on our
#' \href{https://cran.r-project.org/package=cointReg}{\code{cointReg}} package.
#'
#'
#' @docType package
#' @name cointmonitoR-package
NULL

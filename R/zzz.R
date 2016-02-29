#' @import cointReg
#' @importFrom matrixStats colCumsums
#' @importFrom matrixStats colDiffs

.onAttach <- function(libname, pkgname) {
  psm = "Consistent Monitoring of Stationarity and Cointegrating Relationships."
  packageStartupMessage(psm)
}

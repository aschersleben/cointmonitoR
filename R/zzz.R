#' @import stats
#' @import graphics
#' @import cointReg
#' @importFrom matrixStats colCumsums colDiffs

.onAttach <- function(libname, pkgname) {
  psm = paste("cointmonitoR",
              paste0("(v", utils::packageVersion("cointmonitoR"), "):"),
              "Consistent Monitoring of Stationarity",
              "and Cointegrating Relationships.")
  packageStartupMessage(psm)
}

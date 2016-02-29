#' Plot Method for Monitoring Procedures.
#'
#' Plotting objects of class \code{"cointmonitoR"}.
#'
#' @param x [\code{cointmonitoR}]\cr
#'   Object of class \code{"cointmonitoR"}, i.e. the result of
#'   \code{\link{monitorStationarity}} or
#'   \code{\link{monitorCointegration}}.
#'
#' @param what [\code{character}]\cr
#'   Whether to plot test statistics (\code{"test"}) (default) or the
#'   values/residuals of the tested time series (\code{"values"} or
#'   \code{"residuals"}) or \code{"both"}. Works only,
#'   if \code{return.stats = TRUE} in the called function that to get \code{x}
#'   (default setting).
#'
#' @param type [\code{character}]\cr
#'   Plot type (from \code{\link{plot}}). Default is \code{"l"}.
#'
#' @param main,xlab,ylab [\code{character}]\cr
#'   Title and axis titles (from \code{\link{plot}}). Default values will be
#'   generated from the contents of \code{x}.
#'
#' @param axes,legend [\code{logical}]\cr
#'   Whether to add axes (from \code{\link{plot}}) and a legend to the plot.
#'
#' @param main.val,xlab.val,ylab.val [\code{character}]\cr
#'   Title and axis titles (from \code{\link{plot}}) for the second plot,
#'   if generating both plots in one step (see argument \code{what}).
#'   Default values will be generated from the contents of \code{x}.
#'
#' @param lines [\code{logical}]\cr
#'   Whether to add lines and annotations to the plot. Default is \code{TRUE}.
#'
#' @param ... [\code{any}]\cr
#'   Further arguments passed to \code{\link{plot}}.
#'
#' @family cointmonitoR
#'
#' @examples
#' ### Monitoring stationarity (no break):
#' set.seed(1909)
#' x = rnorm(200)
#' test = monitorStationarity(x, m = 0.5)
#' plot(test)
#'
#' oldpar = par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
#' plot(test, what = "both", legend = FALSE, main = "", main.val = "")
#' par(oldpar)
#'
#'
#' ### Monitoring stationarity (break):
#' x = c(x[1:100], cumsum(rnorm(100, sd = 0.5)) + x[101:200])
#' test2 = monitorStationarity(x, m = 0.5)
#' plot(test2)
#'
#' oldpar = par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
#' plot(test2, what = "both", legend = FALSE, main = "", main.val = "")
#' par(oldpar)
#'
#'
#' ### Monitoring cointegration (no break):
#' set.seed(42)
#' x = data.frame(x1 = cumsum(rnorm(200)), x2 = cumsum(rnorm(200)))
#' eps1 = rnorm(200, sd = 2)
#' y = x$x1 - x$x2 + 10 + eps1
#' test3 = monitorCointegration(x = x, y = y, m = 0.5, model = "FM")
#' plot(test3)
#'
#' oldpar = par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
#' plot(test3, what = "both", legend = FALSE, main = "", main.val = "")
#' par(oldpar)
#'
#'
#' ### Monitoring cointegration (break):
#' eps2 = c(eps1[1:100], cumsum(eps1[101:200]))
#' y = x$x1 - x$x2 + 10 + eps2
#' test4 = monitorCointegration(x = x, y = y, m = 0.5, model = "FM")
#' plot(test4)
#'
#' oldpar = par(mfrow = c(2, 1), mar = c(4, 4, 1, 1))
#' plot(test4, what = "both", legend = FALSE, main = "", main.val = "")
#' par(oldpar)
#'
#' @export


plot.cointmonitoR <- function(x, what = "test", type, main, xlab, ylab,
                              axes = TRUE, legend = TRUE,
                              main.val, xlab.val, ylab.val, lines = TRUE, ...) {

  if (!(class(x) == "cointmonitoR"))
    stop("Argument x must be of type \"cointmonitoR\".")

  firstToCap <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

  what <- match.arg(what, c("test", "values", "residuals", "both"),
                    several.ok = TRUE)
  if ("both" %in% what) {
    what <- c("test", "values")
  }

  calib.col <- 4
  calib.lty <- 2

  crit.col <- "grey"
  crit.lty <- 2

  break.col <- 2
  break.lty <- 1

  if (missing(type))
    type <- "l"

  mon.trend <- firstToCap(x$trend)
  mon.type <- firstToCap(attr(x, "type"))

  if (missing(main))
    main <- paste("Monitoring Procedure for", mon.trend, mon.type)

  if (missing(xlab))
    xlab <- "Observation Number"

  if ("test" %in% what) {
    if (missing(ylab))
      ylab <- paste("Test Statistics (Hsm)")

    plot.default(x$statistics, type = type, main = main, xlab = xlab,
                 ylab = ylab, axes = axes, ...)
    # points(c(0, x$m$m.index), c(0, 0), type = "b", col = 2, pch = 19)
    if (lines) abline(v = c(0, x$m$m.index), col = calib.col, lty = calib.lty)
    # text(x$m$m.index / 2, 0, "Calibration", pos = 3, col = 2)

    if (x$p.value <= x$sig) {
      if (lines) {
        abline(h = x$cv, col = crit.col, lty = crit.lty)
        abline(v = x$time, col = break.col, lty = break.lty)
      }
      if (legend) {
        legend("topleft", col = c(calib.col, crit.col, break.col),
               lty = c(calib.lty, crit.lty, break.lty),
               legend = c("Calibration Period", "Critical Value",
                          paste0("Detected Break Point (", x$time, ")")))
      }
    } else {
      if (lines) {
        text(x$m$m.index / 2, par("usr")[3], "Calibration", pos = 3,
             col = calib.col, lty = calib.lty)
      }
    }
  }

  if ("values" %in% what | "residuals" %in% what) {
    if (missing(ylab.val)) {
      if ("test" %in% what || missing(ylab))
        ylab.val <- x$name
      else ylab.val <- ylab
    }

    if (mon.type == "Stationarity") {
      plot.default(as.numeric(x$input), type = type, main = main, xlab = xlab,
                   ylab = ylab.val, ...)
    } else {
      plot.default(x$residuals, type = type, main = main, xlab = xlab,
                   ylab = ylab.val, ...)
    }

    if (lines) abline(v = c(0, x$m$m.index), col = calib.col, lty = calib.lty)

    if (x$p.value <= x$sig) {
      if (lines) abline(v = x$time, col = break.col, lty = break.lty)
      if (legend) {
        legend("topleft", col = c(calib.col, break.col),
               lty = c(calib.lty, break.lty),
               legend = c("Calibration Period",
                          paste0("Detected Break Point (", x$time, ")")))
      }
    } else {
      if (lines) {
        text(x$m$m.index / 2, par("usr")[3], "Calibration", pos = 3,
             col = calib.col, lty = calib.lty)
      }
    }
  }
}

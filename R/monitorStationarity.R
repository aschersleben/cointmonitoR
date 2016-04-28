#' Procedure for Monitoring Level and Trend Stationarity
#'
#' This procedure is able to monitor a one-dimensional vector for level or
#' trend stationarity and returns the corresponding break point, if available.
#' It is based on parameter estimation on a pre-break "calibration" period
#' at the beginning of the sample that is known or assumed to be free of
#' structural change and can be specified exactly via the \code{m} argument
#' (see Details for further information).
#'
#' @param x [\code{numeric} | \code{matrix} | \code{data.frame}]\cr
#'   Data on which to apply the monitoring procedure. If \code{matrix}, it may
#'   have only one row or column, if \code{data.frame} just one column.
#'
#' @param m [\code{numeric(1)}]\cr
#'   Length of calibration period as fraction of the data's length
#'   (between 0.1 and 0.9) or as number of observations (see Details).
#'
#' @param trend [\code{logical}]\cr
#'   Should an intercept and a linear trend be included?
#'   If \code{FALSE} (default), only an intercept is included.
#'
#' @param kernel [\code{character(1)}]\cr
#'   The kernel function to use for calculating the long-run variance.
#'   Default is Bartlett kernel (\code{"ba"}), see Details for alternatives.
#'
#' @param bandwidth [\code{character(1)} | \code{numeric(1)}]\cr
#'   The bandwidth to use for calculating the long-run variance.
#'   Default is Andrews (1991) (\code{"and"}), an alternative is Newey West
#'   (1994) (\code{"nw"}). You can also set the bandwidth manually.
#'
#' @param signif.level [\code{numeric(1)}]\cr
#'   Level of significance (between 0.01 and 0.1).
#'   Detection time will be calculated only if the estimated
#'   p-value is smaller than \code{signif.level}. Default is 0.05.
#'
#' @param return.stats [\code{logical}]\cr
#'   Whether to return all test statistics. Default is \code{TRUE}.
#'
#' @param return.input [\code{logical}]\cr
#'   Whether to return the input data, default is \code{TRUE}.
#'
#' @param check [\code{logical}]\cr
#'   Wheather to check (and if necessary convert) the arguments.
#'   See \code{\link[cointReg]{checkVars}} for further information.
#'
#' @param ...
#'   Arguments passed to \code{\link[cointReg]{getBandwidthNW}} (\code{inter},
#'   \code{weights}), if \code{bandwidth = "nw"}.
#'
#' @details
#'
#' The calibration period can be specified by setting the argument \code{m}
#' to the number of its last observation.
#' The corresponding fraction of the data's length will be calculated
#' automatically. Alternatively you can set \code{m} directly to the fitting
#' fraction value. Attention: The calibration period may become smaller than
#' intended: The last observation is calculated as \code{floor(m * N)}
#' (with \code{N} = length of \code{x}).
#'
#' The kernel that is used for calculating the long-run variance can be
#' one of the following:
#' \itemize{
#'   \item \code{"ba"}: Bartlett kernel
#'   \item \code{"pa"}: Parzen kernel
#'   \item \code{"qs"}: Quadratic Spectral kernel
#'   \item \code{"tr"}: Truncated kernel
#' }
#'
#' @return [\code{cointmonitoR}] object with components:
#' \describe{
#'   \item{\code{Hsm} [\code{numeric(1)}]}{
#'     value of the test statistic}
#'
#'   \item{\code{time} [\code{numeric(1)}]}{
#'     detected time of structural break}
#'
#'   \item{\code{p.value} [\code{numeric(1)}]}{
#'     estimated p-value of the test (between 0.01 and 0.1)}
#'
#'   \item{\code{cv} [\code{numeric(1)}]}{
#'     critical value of the test}
#'
#'   \item{\code{sig} [\code{numeric(1)}]}{
#'     significance level used for the test}
#'
#'   \item{\code{trend} [\code{character(1)}]}{
#'     trend model ("level" or "trend")}
#'
#'   \item{\code{name} [\code{character(1)}]}{
#'     name(s) of data}
#'
#'   \item{\code{m} [\code{list(2)}]}{
#'     list with components:\cr
#'     \code{$m.frac} [\code{numeric(1)}]: calibration period (fraction)\cr
#'     \code{$m.index} [\code{numeric(1)}]: calibration period (length)}
#'
#'   \item{\code{kernel} [\code{character(1)}]}{
#'     kernel function}
#'
#'   \item{\code{bandwidth} [\code{list(2)}]}{
#'     \code{$name} [\code{character(1)}]: bandwidth function (name)\cr
#'     \code{$number} [\code{numeric(1)}]: bandwidth}
#'
#'   \item{\code{statistics} [\code{numeric}]}{
#'     values of test statistics with the same length as data, but \code{NA}
#'     during calibration period (available if \code{return.stats = TRUE})}
#'
#'   \item{\code{input} [\code{numeric} | \code{matrix} | \code{data.frame}]}{
#'     copy of input data (available if \code{return.stats = TRUE})}
#' }
#'
#' @family cointmonitoR
#'
#' @examples
#' set.seed(1909)
#' x <- rnorm(200)
#' x2 <- c(x[1:100], cumsum(x[101:200]) / 2)
#'
#' # Specify the calibration period
#' # as fraction of the total length of x:
#' monitorStationarity(x, m = 0.25)
#' monitorStationarity(x2, m = 0.465)
#'
#' # Specify the calibration period
#' # by setting its last observation exactly:
#' monitorStationarity(x, m = 50)
#' monitorStationarity(x2, m = 93)
#'
#' @export


monitorStationarity <- function(x, m = 0.25, trend = FALSE,
                                kernel = c("ba", "pa", "qs", "tr"),
                                bandwidth = c("and", "nw"),
                                signif.level = 0.05, return.stats = TRUE,
                                return.input = TRUE, check = TRUE, ...) {

  x.name <- deparse(substitute(x))

  # check arguments
  if (check) {
    env <- environment()
    cointReg::checkVars(m = m, trend = trend, kernel = kernel,
                        bandwidth = bandwidth, signif.level = signif.level,
                        return.stats = return.stats,
                        return.input = return.input, .env = env)
    x <- cointReg::checkObject(x.stat = x)
  }

  x.T <- nrow(x)

  if (m >= 1 && m <= (0.9 * x.T) && m >= (0.1 * x.T)) {
    m.index <- floor(m)
  } else if (m < 1 && m <= 0.9 && m >= 0.1) {
    m.index <- floor(m * x.T)
  } else {
    stop("m out of possible range.", call. = FALSE)
  }
  m.frac <- m.index / x.T
  if (m != m.frac && m != m.index) {
    warning("Specified parameter m: Observation no. ", m.index,
            " is the end of the calibration period (m = ",
            format(m.frac) ,").", call. = FALSE)
  }

  if (trend) {
    data <- data.frame(x = x, trend = 1:x.T)
    reg <- lm(x ~ 1 + trend, subset = 1:m.index, data = data)
    u <- x - predict(reg, newdata = data)
    ex <- 5
    trend.name <- "trend"
  } else {
    u <- x - mean(x[(1:m.index), ])
    ex <- 3
    trend.name <- "level"
  }

  S <- cumsum(u)
  S.dev <- (S / sqrt(x.T))^2 / x.T
  cumsum.ms <- cumsum(S.dev[(m.index + 1):x.T])
  sum.1m <- sum(S.dev[1:m.index])

  if(!is.numeric(bandwidth)) {
    bw <- cointReg::getBandwidth(u[1:m.index, , drop = FALSE], kernel = kernel,
                                 bandwidth = bandwidth, check = FALSE)
    bandwidth <- switch(bandwidth, and = "Andrews", nw = "Newey-West")
  } else {
    bw <- bandwidth
    bandwidth <- "set by user"
  }
  omega <- cointReg::getLongRunVar(u = u[1:m.index, , drop = FALSE],
                                   kernel = kernel, bandwidth = bw,
                                   demeaning = FALSE, check = FALSE)
  omega <- as.numeric(omega$Omega)

  erg <- (cumsum.ms - sum.1m) / omega
  s <- seq(m.frac, 1, length = x.T - m.index)
  w <- s^ex

  stats.all <- c(rep(NA, m.index), abs(erg / w))
  Hsm <- max(stats.all, na.rm = TRUE)

  cv.table <- cv.stat[[trend.name]]
  cv.which <- (cv.table[, 1] == m.frac)
  if (any(cv.which)) {
    cv.tab <- cv.table[cv.which, 2:5]
  } else {
    cv.tab <- apply(cv.table[, 2:5], 2, function(x) {
      spline(cv.table[, 1], x, xout = m.frac)$y
    })
  }

  p.table <- c(0.1, 0.05, 0.025, 0.01)
  p.value <- spline(cv.tab, p.table, xout = Hsm, method = "natural")$y
  if (p.value < min(p.table)) {
    p.value <- min(p.table)
  } else if (p.value > max(p.table)) {
    p.value <- max(p.table)
  }

  if (signif.level %in% p.table) {
    critical.value <- cv.tab[p.table %in% signif.level]
  } else {
    critical.value <- spline(p.table, cv.tab, xout = signif.level,
                             method = "natural")$y
  }
  critical.value <- as.numeric(critical.value)

  if (Hsm > critical.value) {
    rej.time <- min(which(stats.all > critical.value))
  } else {
    rej.time <- Inf
  }

  out <- list(Hsm = Hsm, time = rej.time,
              p.value = p.value, cv = critical.value,
              sig = signif.level, trend = trend.name,
              name = x.name, m = list(m.frac = m.frac, m.index = m.index),
              kernel = kernel, bandwidth = list(name = bandwidth, number = bw))

  if(return.stats) {
    out <- c(out, statistics = list(stats.all))
  }

  if(return.input) {
    out <- c(out, input = list(x))
  }

  class(out) <- "cointmonitoR"
  attr(out, "type") <- "stationarity"
  return(out)
}

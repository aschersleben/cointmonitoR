#' Procedure for Monitoring Level and Trend Cointegration
#'
#' This procedure is able to monitor a cointegration model for level or
#' trend cointegration and returns the corresponding break point, if available.
#' It is based on parameter estimation on a pre-break "calibration" period
#' at the beginning of the sample that is known or assumed to be free of
#' structural change and can be specified exactly via the \code{m} argument
#' (see Details for further information).
#'
#' @param x [\code{numeric} | \code{matrix} | \code{data.frame}]\cr
#'   Data on which to apply the monitoring procedure (RHS).
#'
#' @param y [\code{numeric} | \code{matrix} | \code{data.frame}]\cr
#'   Data on which to apply the monitoring procedure (LHS).
#'   Has to be one-dimensional. If \code{matrix}, it may
#'   have only one row or column, if \code{data.frame} just one column.
#'
#' @param model [\code{character(1)}]\cr
#'   The model to be used for modified OLS calculations. Should be one of
#'   FM-OLS (\code{"FM"}), D-OLS (\code{"D"}) or IM-OLS (\code{"IM"}).
#'
#' @param D.options [\code{list} | \code{NULL}]\cr
#'   Options for the D-OLS calculations. A list with elements \code{n.lead},
#'   \code{n.lag}, \code{kmax} and \code{info.crit} -- or \code{NULL} (then
#'   default arguments are the same as in \code{\link[cointReg]{cointRegD}}.
#'   See that help page for further information.)
#'   Missing list elements will be replaced automatically.
#'
#' @inheritParams monitorStationarity
#'
#' @details
#' The calibration period can be set by setting the argument \code{m} to the
#' number of the last observation, that should be inside this period.
#' The corresponding fraction of the data's length will be calculated
#' automatically. Alternatively you can set \code{m} directly to the fitting
#' fraction value, but you should pay attention to the fact, that the
#' calibration period may become smaller than intended: The last observation
#' is calculated as \code{floor(m * N)} (with \code{N} the length of x).
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
#'   \item{\code{residuals} [\code{numeric}]}{
#'     residuals of the modified OLS model to be used for calculating the
#'     test statistics}
#'
#'   \item{\code{model} [\code{character(1)}]}{
#'     \code{cointOLS} model ("FM", "D", or "IM")}
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
#'
#'   \item{\code{D.options} [\code{list}]}{
#'     information about further parameters (available if \code{model = "D"})}
#' }
#'
#' @family cointmonitoR
#'
#' @examples
#' set.seed(42)
#' x = data.frame(x1 = cumsum(rnorm(200)), x2 = cumsum(rnorm(200)))
#' eps1 = rnorm(200, sd = 2)
#' eps2 = c(eps1[1:100], cumsum(eps1[101:200]))
#'
#' y = x$x1 - x$x2 + 10 + eps1
#' monitorCointegration(x = x, y = y, m = 0.5, model = "FM")
#'
#' y2 = y + seq(1, 30, length = 200)
#' monitorCointegration(x = x, y = y2, m = 0.5, model = "FM")
#' monitorCointegration(x = x, y = y2, m = 0.5, trend = TRUE, model = "FM")
#'
#' y3 = x$x1 - x$x2 + 10 + eps2
#' monitorCointegration(x = x, y = y3, m = 0.5, model = "FM")
#' monitorCointegration(x = x, y = y3, m = 0.5, model = "D")
#' monitorCointegration(x = x, y = y3, m = 0.5, model = "IM")
#'
#' @export


monitorCointegration <- function(x, y, m = 0.25, model = c("FM", "D", "IM"),
                                 trend = FALSE,
                                 kernel = c("ba", "pa", "qs", "tr"),
                                 bandwidth = c("and", "nw"), D.options = NULL,
                                 signif.level = 0.05, return.stats = TRUE,
                                 return.input = TRUE, check = TRUE, ...) {

  mod.name <- paste(deparse(substitute(y)), "~", deparse(substitute(x)))

  # check arguments
  if (check) {
    env <- environment()
    checkVars(y = y, m = m, model = model, trend = trend,
              kernel = kernel, bandwidth = bandwidth,
              signif.level = signif.level, return.stats = return.stats,
              return.input = return.input, .env = env)
    x <- checkObject(x.coint = x)
  }

  x.T <- nrow(x)
  if (ncol(x) > 4) {
    warning("Four regressors at most. Only first ones will be used.",
            call. = FALSE)
    x <- x[, 1:4]
  }
  x.k <- ncol(x)

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
    deter <- cbind(level = 1, trend = 1:x.T)
    ex <- 5
    trend.name <- "trend"
  } else {
    deter <- cbind(level = rep(1, x.T))
    ex <- 3
    trend.name <- "level"
  }

  y.m <- y[1:m.index, , drop = FALSE]
  x.m <- x[1:m.index, , drop = FALSE]
  deter.m <- deter[1:m.index, , drop = FALSE]

  FM.m <- cointRegFM(x = x.m, y = y.m, deter = deter.m, kernel = kernel,
                     bandwidth = bandwidth, demeaning = FALSE,
                     check = FALSE, ...)
  omega <- as.numeric(FM.m$omega.u.v)
  bw <- FM.m$bandwidth$number
  band <- FM.m$bandwidth$name

  if (model == "FM") {
    x.delta <- diff(x, lag = 1, differences = 1)
    y.plus <- as.numeric(
      y[-1, ] - x.delta %*% cointReg:::trySolve(FM.m$Omega$vv) %*% t(FM.m$Omega$uv))
    u.fm <- as.numeric(y.plus - cbind(deter, x)[-1, ] %*% FM.m$theta)
    u <- c(0, u.fm)
    u.out <- c(NA, u.fm)
    coint.mod <- FM.m
  }

  if (model == "D") {

    if(!is.list(D.options))
      D.options <- list()

    D.m <- cointRegD(x = x.m, y = y.m, deter = deter.m,
                     n.lead = D.options$n.lead, n.lag = D.options$n.lag,
                     kmax = D.options$kmax, D.options$info.crit,
                     kernel = kernel, bandwidth = bw,
                     demeaning = FALSE, check = FALSE)
    coint.mod <- D.m
    n.lead <- D.m$lead.lag$n.lead
    n.lag <- D.m$lead.lag$n.lag
    Z <- cbind(deter, x)

    if(n.lag + n.lead == 0) {
      all.trunc <- Z
      y.trunc <- y
      u.d <- y.trunc - all.trunc %*% D.m$theta.all
      u <- u.out <- u.d
    } else {
      x.delta <- colDiffs(x)
      dx.all <- cointReg:::makeLeadLagMatrix(x.delta, n.lag, n.lead)
      Zs <- Z[-1, , drop = FALSE]
      all.untrunc <- cbind(Zs, dx.all)
      T1 <- nrow(all.untrunc)
      all.trunc <- all.untrunc[(n.lag + 1):(T1 - n.lead), , drop = FALSE]
      ys <- y[-1, , drop = FALSE]
      T2 <- nrow(ys)
      y.trunc <- ys[(n.lag + 1):(T2 - n.lead), , drop = FALSE]
      u.d <- y.trunc - all.trunc %*% D.m$theta.all
      u <- c(0, rep(0, n.lag), u.d, c(rep(0, n.lead)))
      u.out <- c(NA, rep(NA, n.lag), u.d, c(rep(NA, n.lead)))
    }
  }

  if (model == "IM") {
    IM.m <- cointRegIM(y = y.m, x = x.m, deter = deter.m, selector = 1,
                       t.test = FALSE, kernel = kernel, bandwidth = bw,
                       check = FALSE)
    deter.cs <- colCumsums(deter)
    x.cs <- colCumsums(x)
    multip <- cbind(deter.cs, x.cs, x)
    S <- as.numeric(cumsum(y) - multip %*% IM.m$theta.all)
    u.im <- diff(S)
    u <- c(0, u.im)
    u.out <- c(NA, u.im)
    cv.val <- "IM"
    coint.mod <- IM.m
  } else {
    S <- cumsum(u)
    cv.val <- "FMD"
  }

  S.dev <- (S / sqrt(x.T))^2 / x.T
  cumsum.ms <- cumsum(S.dev[(m.index + 1):x.T])
  sum.1m <- sum(S.dev[1:m.index])

  erg <- (cumsum.ms - sum.1m) / omega
  s <- seq(m.frac, 1, length = x.T - m.index)
  w <- s^ex

  stats.all <- c(rep(NA, m.index), abs(erg / w))
  Hsm <- max(stats.all, na.rm = TRUE)

  cv.table <- cv.coint[[cv.val]][[trend.name]][[x.k]]
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

  out <- list(Hsm = Hsm, time = rej.time, p.value = p.value,
              cv = critical.value, sig = signif.level,
              residuals = u.out, model = model, trend = trend.name,
              name = mod.name, coint.mod = coint.mod,
              m = list(m.frac = m.frac, m.index = m.index),
              kernel = kernel, bandwidth = list(name = band, number = bw))

  if (return.stats) {
    out$statistics <- stats.all
  }

  if (return.input) {
    out$input <- list(x = x, y = y)
  }

  if (model == "D") {
    out$D.options <- D.m$lead.lag
  }

  class(out) <- "cointmonitoR"
  attr(out, "type") <- "cointegration"
  return(out)
}

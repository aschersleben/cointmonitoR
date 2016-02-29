#' Print Method for Monitoring Procedures.
#'
#' Printing objects of class \code{"cointmonitoR"}.
#'
#' @param x [\code{cointmonitoR}]\cr
#'   Object of class \code{"cointmonitoR"}, i.e. the result of
#'   \code{monitorStationarity()} or \code{monitorCointegration()}.
#' @param ... ignored
#' @param digits [\code{numeric}]\cr
#'   Number of significant digits to be used.
#'
#' @return
#'   The invisible \code{x} object.
#'
#' @family cointmonitoR
#'
#' @examples
#' set.seed(42)
#' test = monitorStationarity(rnorm(100), m = 0.5)
#' print(test)
#'
#' x = data.frame(x1 = cumsum(rnorm(200)), x2 = cumsum(rnorm(200)))
#' eps1 = rnorm(200, sd = 2)
#' eps2 = c(eps1[1:100], cumsum(eps1[101:200]))
#' y1 = x$x1 - x$x2 + 10 + eps1
#' y2 = x$x1 - x$x2 + 10 + eps2
#' test1 = monitorCointegration(x = x, y = y1, m = 0.5, model = "FM")
#' print(test1)
#' test2 = monitorCointegration(x = x, y = y2, m = 0.5, model = "FM")
#' print(test2)
#'
#' @export


print.cointmonitoR <- function(x, ..., digits = getOption("digits")) {

  if (!("cointmonitoR" %in% class(x)))
    stop("Argument x must be of type \"cointmonitoR\".")

  firstToCap <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))

  mon.trend <- firstToCap(x$trend)
  mon.type <- firstToCap(attr(x, "type"))

  cat(paste0("\n", "### Monitoring Procedure for"),
      mon.trend, mon.type, "###\n")

  cat("\nModel:\t")
  cat(paste0("\t",  x$name, "\n"))

  cat("\nParameters:")
  cat(paste0("\t", "m ="), format(x$m[["m.frac"]], digits = digits),
      "(last observation used for calibration:",
      paste0(x$m[["m.index"]], ") \n"))
  cat(ifelse(mon.type != "Stationarity",
             paste0("\t\t", "Model = ",  paste0(x$model, "-OLS"), "  //  ",
                    "Kernel = \"", x$kernel, "\""),
             paste0("\t\t", "Kernel = \"", x$kernel, "\"")),
      " //  Bandwidth =", format(x$bandwidth$number, digits = digits),
      paste0("(", x$bandwidth$name, ")\n"))
  if(!is.null(x$D.options)) {
    cat(paste0("\t\t", "Leads ="), x$D.options$n.lead,
        "/ Lags =", x$D.options$n.lag, "\n")
  }

  cat("\nResults:")
  cat(paste0("\t", "Test Statistic (Hsm) ="),
      format(x$Hsm, digits = digits), "\n")
  cat(paste0("\t\t", "p-Value"),
      ifelse(x$p.value <= 0.01, "<=", ifelse(x$p.value >= 0.1, ">=", "=")),
      format(x$p.value, digits = digits),
      paste0(ifelse(x$p.value > x$sig, "(not ", "("), "significant to"),
      format(x$sig, digits = digits), "level)\n")
  if(x$p.value <= x$sig)
    cat(paste0("\t\t", "Rejection Time ="), x$time, "\n")

  return(invisible(x))
}

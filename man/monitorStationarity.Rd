% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitorStationarity.R
\name{monitorStationarity}
\alias{monitorStationarity}
\title{Procedure for Monitoring Level and Trend Stationarity}
\usage{
monitorStationarity(x, m = 0.25, trend = FALSE, kernel = c("ba", "pa",
  "qs", "tr"), bandwidth = c("and", "nw"), signif.level = 0.05,
  return.stats = TRUE, return.input = TRUE, check = TRUE, ...)
}
\arguments{
\item{x}{[\code{numeric} | \code{matrix} | \code{data.frame}]\cr
Data on which to apply the monitoring procedure. If \code{matrix}, it may
have only one row or column, if \code{data.frame} just one column.}

\item{m}{[\code{numeric(1)}]\cr
Length of calibration period as fraction of the data's length
(between 0.1 and 0.9) or as number of observations (see Details).}

\item{trend}{[\code{logical}]\cr
Should an intercept and a linear trend be included?
If \code{FALSE} (default), only an intercept is included.}

\item{kernel}{[\code{character(1)}]\cr
The kernel function to use for calculating the long-run variance.
Default is Bartlett kernel (\code{"ba"}), see Details for alternatives.}

\item{bandwidth}{[\code{character(1)} | \code{numeric(1)}]\cr
The bandwidth to use for calculating the long-run variance.
Default is Andrews (1991) (\code{"and"}), an alternative is Newey West
(1994) (\code{"nw"}). You can also set the bandwidth manually.}

\item{signif.level}{[\code{numeric(1)}]\cr
Level of significance (between 0.01 and 0.1).
Detection time will be calculated only if the estimated
p-value is smaller than \code{signif.level}. Default is 0.05.}

\item{return.stats}{[\code{logical}]\cr
Whether to return all test statistics. Default is \code{TRUE}.}

\item{return.input}{[\code{logical}]\cr
Whether to return the input data, default is \code{TRUE}.}

\item{check}{[\code{logical}]\cr
Wheather to check (and if necessary convert) the arguments.
See \code{\link[cointReg]{checkVars}} for further information.}

\item{...}{Arguments passed to \code{\link[cointReg]{getBandwidthNW}} (\code{inter},
\code{weights}), if \code{bandwidth = "nw"}.}
}
\value{
[\code{cointmonitoR}] object with components:
\describe{
  \item{\code{Hsm} [\code{numeric(1)}]}{
    value of the test statistic}

  \item{\code{time} [\code{numeric(1)}]}{
    detected time of structural break}

  \item{\code{p.value} [\code{numeric(1)}]}{
    estimated p-value of the test (between 0.01 and 0.1)}

  \item{\code{cv} [\code{numeric(1)}]}{
    critical value of the test}

  \item{\code{sig} [\code{numeric(1)}]}{
    significance level used for the test}

  \item{\code{trend} [\code{character(1)}]}{
    trend model ("level" or "trend")}

  \item{\code{name} [\code{character(1)}]}{
    name(s) of data}

  \item{\code{m} [\code{list(2)}]}{
    list with components:\cr
    \code{$m.frac} [\code{numeric(1)}]: calibration period (fraction)\cr
    \code{$m.index} [\code{numeric(1)}]: calibration period (length)}

  \item{\code{kernel} [\code{character(1)}]}{
    kernel function}

  \item{\code{bandwidth} [\code{list(2)}]}{
    \code{$name} [\code{character(1)}]: bandwidth function (name)\cr
    \code{$number} [\code{numeric(1)}]: bandwidth}

  \item{\code{statistics} [\code{numeric}]}{
    values of test statistics with the same length as data, but \code{NA}
    during calibration period (available if \code{return.stats = TRUE})}

  \item{\code{input} [\code{numeric} | \code{matrix} | \code{data.frame}]}{
    copy of input data (available if \code{return.stats = TRUE})}
}
}
\description{
This procedure is able to monitor a one-dimensional vector for level or
trend stationarity and returns the corresponding break point, if available.
It is based on parameter estimation on a pre-break "calibration" period
at the beginning of the sample that is known or assumed to be free of
structural change and can be specified exactly via the \code{m} argument
(see Details for further information).
}
\details{
The calibration period can be specified by setting the argument \code{m}
to the number of its last observation.
The corresponding fraction of the data's length will be calculated
automatically. Alternatively you can set \code{m} directly to the fitting
fraction value. Attention: The calibration period may become smaller than
intended: The last observation is calculated as \code{floor(m * N)}
(with \code{N} = length of \code{x}).

The kernel that is used for calculating the long-run variance can be
one of the following:
\itemize{
  \item \code{"ba"}: Bartlett kernel
  \item \code{"pa"}: Parzen kernel
  \item \code{"qs"}: Quadratic Spectral kernel
  \item \code{"tr"}: Truncated kernel
}
}
\examples{
set.seed(1909)
x <- rnorm(200)
x2 <- c(x[1:100], cumsum(x[101:200]) / 2)

# Specify the calibration period
# as fraction of the total length of x:
monitorStationarity(x, m = 0.25)
monitorStationarity(x2, m = 0.465)

# Specify the calibration period
# by setting its last observation exactly:
monitorStationarity(x, m = 50)
monitorStationarity(x2, m = 93)

}
\references{
\itemize{
    \item Wagner, M. and D. Wied (2015): "Monitoring Stationarity and
          Cointegration," \emph{Discussion Paper},
          \href{http://dx.doi.org/10.2139/ssrn.2624657}{DOI:10.2139/ssrn.2624657}.
  }
}
\seealso{
Other cointmonitoR: \code{\link{monitorCointegration}},
  \code{\link{plot.cointmonitoR}},
  \code{\link{print.cointmonitoR}}
}


# cointmonitoR

Consistent Monitoring of Stationarity and Cointegrating Relationships.

[![Travis-CI Build Status](https://travis-ci.org/aschersleben/cointmonitoR.svg?branch=master)](https://travis-ci.org/aschersleben/cointmonitoR)

* Installation (`cointmonitoR` depends on our [`cointReg`](https://github.com/aschersleben/cointReg) package, so install it first):
```r
devtools::install_github("aschersleben/cointReg", build_vignettes = TRUE)
devtools::install_github("aschersleben/cointmonitoR", build_vignettes = TRUE)
library("cointmonitoR")
```

* Simple example (stationarity, structural break):
```r
set.seed(1909)
eps <- rnorm(200)
x <- c(eps[1:100], cumsum(eps[101:200]) / 2)
test <- monitorStationarity(x, m = 93)
print(test)
oldpar <- par(mfrow = c(2, 1))
plot(test, what = "both", legend = FALSE)
par(oldpar)
```

* Package vignette: Provides further examples and explanations.
```r
vignette("cointmonitoR")
```

* Package help page: Overview of all available functions:
```r
package?cointmonitoR
```

* [Package issues](https://github.com/aschersleben/cointmonitoR/issues)



## Theoretical background

We propose a consistent monitoring procedure to detect a structural change from a cointegrating relationship to a spurious relationship. The procedure is based on residuals from modified least squares estimation, using either Fully Modified, Dynamic or Integrated Modified OLS. It is inspired by Chu et al. (1996) in that it is based on parameter estimation on a pre-break "calibration" period only, rather than being based on sequential estimation over the full sample.

See the discussion paper at http://dx.doi.org/10.2139/ssrn.2624657 for further information.

This package provides the monitoring procedures for both the cointegration and the stationarity case (the latter is just a special case of the former one) as well as printing and plotting methods for a clear presentation of the results.

#' @title  An example of MA theoretical spectral density using Rcpp
#' @name example
#' @description Use R package \code{stats} to generate MA model random number and apply to R functions (\code{ma_true_spectrum}).
#' @examples
#' \dontrun{
#' set.seed(101)
#' xma <- arima.sim(model=list(ma=c(0.5, -0.4)), n = 100, sd = sqrt(4) )
#' ma_true_spectrum(NumericVector xma, int ngrid = 256, double sigma = 1.0)
#' }
#' @import microbenchmark
#' @import RcppArmadillo
#' @import stats
#' @import knitr
#' @import rmarkdown
#' @import boot
#' @import bootstrap
#' @import DAAG
#' @import coda
#' @import microbenchmark
#' @importFrom graphics axis box
#' @importFrom Rcpp evalCpp
#' @useDynLib SA24204133
NULL

#' @title  MA theoretical spectral density using R
#' @description  MA theoretical spectral density using R
#' @param  a MA matrix
#' @param ngrid Interval number
#' @param sigma error term
#' @param tit title of picture
#' @param plot.it whether plot
#' @return frequencies spectrum ma_coefficients sigma
#' @examples
#' \dontrun{
#' set.seed(101)
#'  xma <- arima.sim(model=list(ma=c(0.5, -0.4)), n = 100, sd = sqrt(4) )
#' ma_true_spectrum(NumericVector xma, int ngrid = 256, double sigma = 1.0)
#' }
#' @export
ma.true.spectrum <- function(
    a, ngrid=256, sigma=1,
    tit="True MA Spectral Density",
    plot.it=TRUE){
  p <- length(a)
  freqs <- seq(from=0, to=pi, length=ngrid)
  spec <- numeric(ngrid)
  for(ii in seq_len(ngrid)){
    temp_sum <- 0
    for(jj in seq_len(p)){
      temp_sum <- temp_sum + a[jj] * exp(-1i * freqs[ii] * jj)
    }
    spec[ii] <- sigma^2 / (2 * pi) * abs(temp_sum)^2
  }
  if(plot.it){
    plot(freqs, spec, type='l',
         main=tit,
         xlab="frequency", ylab="spectrum",
         axes=FALSE)
    axis(2)
    axis(1, at=(0:6)/6*pi,
         labels=c(0, pi/6, pi/3, pi/2, 2*pi/3, 5*pi/6, pi))
    box()
  }
  invisible(list(frequencies=freqs, spectrum=spec,
                 ma.coefficients=a, sigma=sigma))
}
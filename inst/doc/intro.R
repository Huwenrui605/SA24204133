## ----eval=FALSE---------------------------------------------------------------
#  ma.true.spectrum <- function(
#      a, ngrid=256, sigma=1,
#      tit="True MA Spectral Density",
#      plot.it=TRUE){
#    p <- length(a)
#    freqs <- seq(from=0, to=pi, length=ngrid)
#    spec <- numeric(ngrid)
#    for(ii in seq(ngrid)){
#      spec[ii] <- 1 + sum(complex(mod=a, arg=freqs[ii]*seq(p)))
#    }
#    spec = sigma^2 / (2*pi) * abs(spec)^2
#    if(plot.it){
#      plot(freqs, spec, type='l',
#           main=tit,
#           xlab="frequency", ylab="spectrum",
#           axes=FALSE)
#      axis(2)
#      axis(1, at=(0:6)/6*pi,
#           labels=c(0, expression(pi/6),
#                    expression(pi/3), expression(pi/2),
#                    expression(2*pi/3), expression(5*pi/6), expression(pi)))
#      box()
#    }
#    invisible(list(frequencies=freqs, spectrum=spec,
#                   ma.coefficients=a, sigma=sigma))
#  }

## ----eval=FALSE---------------------------------------------------------------
#  std::vector<double> armaWold(int n, const std::vector<double>& a, const std::vector<double>& b = std::vector<double>()) {
#    int p = a.size();
#    int q = b.size();
#  
#    std::vector<double> arev(a.rbegin(), a.rend());  // Reversed version of a
#    std::vector<double> psi(n, 0.0);  // Initialize psi with zeros
#    psi[0] = 1.0;  // Set the first element to 1
#  
#    for (int j = 0; j < n - 1; ++j) {
#      double bj = (j < q) ? b[j] : 0.0;  // If j is within b, use b[j], else 0
#      std::vector<double> psis(psi.begin(), psi.begin() + std::min(j + 1, n));
#      int np = psis.size();
#  
#      // If the length of psis is less than p, pad with 0
#      if (np < p) {
#        psis.insert(psis.begin(), p - np, 0.0);
#      }
#  
#      // Compute the next value of psi[j+1]
#      double sum = 0.0;
#      for (int i = 0; i < p; ++i) {
#        sum += arev[i] * psis[i];
#      }
#  
#      psi[j + 1] = bj + sum;
#    }
#  
#    return psi;
#  }

## -----------------------------------------------------------------------------
library(SA24204133)
set.seed(101)
xma <- arima.sim(model=list(ma=c(0.5, -0.4)), n = 100, sd = sqrt(4) )
ma.true.spectrum(xma,256,1.0)

## -----------------------------------------------------------------------------
a = c(0.5,0.3,0.2);
b = c(0.1,0.2);
n = 10;
armaWold(n, a, b)


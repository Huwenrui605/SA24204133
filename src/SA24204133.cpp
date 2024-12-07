#include <Rcpp.h>
#include <algorithm>
#include <numeric>
using namespace Rcpp;

//' @title Calculating Wold coefficient from ARMA model parameters
//' @description Calculating Wold coefficient from ARMA model parameters
//' @param n number
//' @param a AR model coefficient
//' @param b MA model coefficient
//' @return wold coefficient
//' @examples
//' \dontrun{
//' std::vector<double> a = {0.5, 0.3, 0.2};
//' std::vector<double> b = {0.1, 0.2};
//' int n = 10;
//' armaWold(n, a, b)
//' }
//' @export
// [[Rcpp::export]]
std::vector<double> armaWold(int n, const std::vector<double>& a, const std::vector<double>& b = std::vector<double>()) {
  int p = a.size();
  int q = b.size();
  
  std::vector<double> arev(a.rbegin(), a.rend());  // Reversed version of a
  std::vector<double> psi(n, 0.0);  // Initialize psi with zeros
  psi[0] = 1.0;  // Set the first element to 1
  
  for (int j = 0; j < n - 1; ++j) {
    double bj = (j < q) ? b[j] : 0.0;  // If j is within b, use b[j], else 0
    std::vector<double> psis(psi.begin(), psi.begin() + std::min(j + 1, n));
    int np = psis.size();
    
    // If the length of psis is less than p, pad with 0
    if (np < p) {
      psis.insert(psis.begin(), p - np, 0.0);
    }
    
    // Compute the next value of psi[j+1]
    double sum = 0.0;
    for (int i = 0; i < p; ++i) {
      sum += arev[i] * psis[i];
    }
    
    psi[j + 1] = bj + sum;
  }
  
  return psi;
}
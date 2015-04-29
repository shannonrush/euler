#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]

std::unordered_set<int> CountTriples(NumericVector primes, int n) {
    std::unordered_set<int> result;
    for (NumericVector::iterator a = primes.begin(); a != primes.end(); ++a) {
        for (NumericVector::iterator b = primes.begin(); b != primes.end(); ++b) {
            if ((pow(*a,4)+pow(*b,3))<n) {
                for (NumericVector::iterator c = primes.begin(); c != primes.end(); ++c) {
                    int r = pow(*a,4)+pow(*b,3)+pow(*c,2);
                    if (r<n) result.insert(r);
                }
            }
        }
    }
    return result;
}




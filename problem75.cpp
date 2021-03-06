#include <Rcpp.h>
using namespace Rcpp;

long gcd(long a, long b) {
    long y, x;
    
    if (a > b) {
        x = a;
        y = b;
    } else {
        x = b;
        y = a;
    }
    
    while (x % y != 0) {
        long temp = x;
        x = y;
        y = temp % x;
    }
    
    return y;
}

// [[Rcpp::export]]

NumericVector PTCounts(int L) {
    NumericVector counts(L+1);
    for (long m=2; m<=sqrt(L/2); m++) {
        for (long n=1; n<m; n++) {
            if (((n + m) % 2) == 1 && gcd(n, m) == 1) {
                long a = m*m + n*n;
                long b = m*m - n*n;
                long c = 2*m*n;
                int l = a+b+c;
                while (l<=L) {
                    counts[l]++;
                    l = l+a+b+c;
                }
            }
        }
    }
    return counts;
}


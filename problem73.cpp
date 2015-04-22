#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector findnext(int a, int b, int c, int d, long n) { 
    NumericVector out(2);
    int f = floor((n+b)/d);
    out[0] = f*c-a;
    out[1] = f*d-b;
    return out;
}

// [[Rcpp::export]] 
long countadj(int a, int b, int p, int q, long n) {
    long count = 1;
    while (!(p==1 && q==2)) {
        NumericVector adj = findnext(a, b, p, q, n);
        a = p;
        b = q;
        p = adj[0];
        q = adj[1];
        count++;
    }
    return count-1;
}

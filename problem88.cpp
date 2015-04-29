#include <Rcpp.h>
using namespace Rcpp;

std::vector<int> n;

void ProdSum(int p, int s, int c, int start, int k_max) {
    int k = (p-s+c)-1;
    if (k<k_max) {
        if (p<n[k]) n[k]=p;
        for (int i=start; i<k_max/p*2; i++) {
            ProdSum(p*i, s+i, c+1, i, k_max);
        }
    }
}

// [[Rcpp::export]]

std::vector<int> ProductSum(int k_max) {
    n.clear();
    for (int i=1; i<=k_max; i++) n.push_back(2*k_max);
    ProdSum(1,1,1,2,k_max);
    return n;
}

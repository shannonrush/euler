#include <Rcpp.h>
using namespace Rcpp;

int Factorial(int n) {
    return (n == 1 || n == 0) ? 1 : Factorial(n - 1) * n;
}

int NumDigits(int n) {
    int length = 1;
    while (n /= 10) length++;
    return(length);
}

NumericVector IntToDigits(int n) {
    int num_digits = NumDigits(n);
    int i = 0;
    NumericVector a(num_digits); 
    while (n) { 
        a[i++] = n % 10; 
        n /= 10;
    }
    std::reverse(a.begin(),a.end());
    return(a);
}

bool InVector(std::vector<int> v, int n) {
    for (int i=0; i < v.size(); i++) {
        if (v[i]==n) return true;
    }
    return false;
}

// [[Rcpp::export]]
int ChainLength(int n) {
    std::vector<int> chain;
    chain.reserve(60);
    while (!InVector(chain, n)) {
        chain.push_back(n);
        NumericVector digits = IntToDigits(n);
        n = 0;
        for (int i=0; i < digits.size(); i++) {
            n += Factorial(digits[i]);
        }
    }
    return(chain.size());
}
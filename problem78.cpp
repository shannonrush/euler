#include <Rcpp.h>
#include <stdio.h>
#include <gmp.h>
#include <gmpxx.h>
using namespace Rcpp;

// [[Rcpp::export]]

std::string P(NumericVector k, NumericVector sign, int limit) {
    std::vector<mpz_class> p(limit+1);
    p[0] = 1;
    p[1] = 1;
    p[2] = 2;
    int n = 3;
    while (n<=limit) {
        mpz_class sum = 0;
        int i = 0;
        while (n>=k[i]) {
            sum = sum + p[n-k[i]] * sign[i%4];
            i++;
        }
        p[n] = sum;
        n++;
    }
    mpz_class a = p.back();
    return a.get_str();
}

// # CoinPartitions <- function() {
// #     k <- c(sapply(1:1000, function(x) c(Pent(x),Pent(-x))))
// #     p <- c(as.bigz(c(1,2)))
// #     m<-10^6
// #     n <- 3
// #     while (tail(p,1)%%m!=0) {
// #         print(n)
// #         sum <- 0
// #         i <- 1
// #         while (n>=k[i]) {
// #              sign <- ifelse(i%%4 %in% c(1,2), 1, -1)
// #              if (n==k[i]) {
// #                  sum <- sum + 1 * sign
// #              } else {
// #                  sum <- sum + p[n-k[i]] * sign   
// #              }
// #              i <- i+1
// #          }
// #          p <- c(p, sum)
// #          n <- n+1
// #     }
// #     n-1
// # }

# The first two consecutive numbers to have two distinct prime factors are:
#     
# 14 = 2 × 7
# 15 = 3 × 5
# 
# The first three consecutive numbers to have three distinct prime factors are:
#     
# 644 = 2² × 7 × 23
# 645 = 3 × 5 × 43
# 646 = 2 × 17 × 19.
# 
# Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
library(gmp)

DistinctPrimeFactors <- function(n) {
    consec <- c()
    i <- 2
    while (length(consec)!=n) {
        if (HasDistinct(i,n)) {
            consec <- c(consec, i)
        } else {
            consec <- c()
        }
        i <- i+1
    }
    consec[1]
}

HasDistinct <- function(i, n) {
    length(unique(factorize(i)))>=n
}

# test
#DistinctPrimeFactors(2)==14
#DistinctPrimeFactors(3)==644
#HasDistinct(14,2)==TRUE

# a <- DistinctPrimeFactors(4)
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# DistinctPrimeFactors(4) 9.771002 9.771002 9.771002 9.771002 9.771002 9.771002     1
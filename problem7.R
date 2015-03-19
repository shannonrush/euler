# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
# 
# What is the 10001st prime number?

NthPrime <- function(n) {
    primes <- c(2,3,5,7)
    i <- 8
    while (length(primes) < n) {
        if (i%%2!=0 & i%%3!=0 & i%%5!=0 & i%%7!=0) {
            if (IsPrime(i)) primes <- c(primes, i) 
        }
        i <- i+1
    }
    tail(primes,1)
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m < max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}

# test
NthPrime(6)==13
# [1] TRUE

answer <- NthPrime(10001)
# [1] 104743

# Unit: microseconds
# expr         min           lq        mean      median          uq         max neval
# NthPrime(11)     124.449     131.8015     137.001     135.909     139.801     169.147   100
# NthPrime(101)    3623.546    3804.9510    4037.429    3852.771    3995.270    5471.694   100
# NthPrime(1001)  122043.323  123975.8030  127232.292  124990.117  126833.376  179182.019   100
# NthPrime(10001) 4071745.173 4120118.9415 4144954.342 4142205.429 4159904.413 4358965.792   100

# O(n^2)
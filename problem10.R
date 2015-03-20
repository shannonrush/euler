# The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
# 
# Find the sum of all the primes below two million.

SumPrimes <- function(n) {
    primes <- c(2,3,5,7)
    for (i in 8:(n-1)) {
        if (i <= 7 || (i%%2!=0 & i%%3!=0 & i%%5!=0 & i%%7!=0)) {
            if (IsPrime(i)) primes <- c(primes, i)  
        }
    }
    sum(primes)
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
SumPrimes(10)==17
# TRUE

SumPrimes(2000000)
#[1] 142913828922

# expr           min            lq          mean        median            uq           max neval
# SumPrimes(10)        15.365        15.365        15.365        15.365        15.365        15.365     1
# SumPrimes(2000000) 380931314.461 380931314.461 380931314.461 380931314.461 380931314.461 380931314.461     1

# O(n^2)
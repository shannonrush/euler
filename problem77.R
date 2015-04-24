# It is possible to write ten as the sum of primes in exactly five different ways:
#     
# 7 + 3
# 5 + 5
# 5 + 3 + 2
# 3 + 3 + 2 + 2
# 2 + 2 + 2 + 2 + 2
# 
# What is the first value which can be written as the sum of primes in over five thousand different ways?

PrimeSums <- function() {
    primes <- PrimesTo(10000)
    n <- 9
    count <- 0
    while (count < 5000) {
        n <- n+1
        count <- PrimeCombos(0, primes[primes<n], 1, n)
    }
    n
}

PrimeCombos <- function(sum, primes, i, n) {
    if (sum==n) {
        return(1)
    } else if (sum>n) {
        return(0)
    } else {
        a <- PrimeCombos(sum+primes[i], primes, i, n)
        b <- ifelse(i+1<=length(primes), PrimeCombos(sum, primes, i+1, n), 0)
        a+b
    }
}

PrimesTo <- function(n) {
    which(sapply(2:n, IsPrime))+1
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m<max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}

# test
#PrimeCombos(0, c(2,3,5,7), 1, 10)==5

#a <- PrimeSums(5000)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PrimeSums() 45.64167 45.64167 45.64167 45.64167 45.64167 45.64167     1
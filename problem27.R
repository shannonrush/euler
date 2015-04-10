# Euler discovered the remarkable quadratic formula:
#     
#     n² + n + 41
# 
# It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. 
# However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, 
# and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
# 
# The incredible formula  n² − 79n + 1601 was discovered, 
# which produces 80 primes for the consecutive values n = 0 to 79. 
# The product of the coefficients, −79 and 1601, is −126479.
# 
# Considering quadratics of the form:
#     
#     n² + an + b, where |a| < 1000 and |b| < 1000
# 
# where |n| is the modulus/absolute value of n
# e.g. |11| = 11 and |−4| = 4
# 
# Find the product of the coefficients, a and b, 
# for the quadratic expression that produces the maximum number of primes for consecutive values of n, 
# starting with n = 0.

QuadPrimes <- function() {
    max.prod <- 0
    max.primes <- 0
    primes <- PrimesTo(5000)
    b.primes <- BPrimes()
    for (a in -999:999) {
        for (b in b.primes) {
            np <- NumPrimes(a,b,primes)
            if (np>max.primes) {
                max.primes <- np
                max.prod <- a*b
            }
        }
    }
    max.prod
}

NumPrimes <- function(a,b,primes) {
    n<-0
    x <- n^2+(a*n)+b
    while (x %in% primes) {
        n <- n+1
        x <- n^2+(a*n)+b
    }
    n
}

PrimesTo <- function(n) {
    p <- c()
    for (n in 2:n) if (IsPrime(n)) p<-c(p,n)
    p
}

BPrimes <- function() {
    b <- c()
    for (n in -999:999) if (IsPrime(abs(n))) b<-c(b,n)
    b
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

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# QuadPrimes() 26.69335 26.69335 26.69335 26.69335 26.69335 26.69335     1
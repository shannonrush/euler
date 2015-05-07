# In the following equation x, y, and n are positive integers.
# 1/x + 1/y = 1/n

# It can be verified that when n = 1260 there are 113 distinct solutions and this is the least value of n 
# for which the total number of distinct solutions exceeds one hundred.
# 
# What is the least value of n for which the number of distinct solutions exceeds four million?

library(gmp)

DiophantineII <- function() {
    L <- 8000000
    primes <- Primes(15)
    exp <- Exponents(13)
    ans <- Inf
    for (i in 1:length(exp)) {
        e <- unlist(exp[[i]])
        if (Reduce("*",e)>L) {
            e <- (e-1)/2
            possible <- prod(sapply(1:14, function(x) primes[x]^(e[x])))
            if (possible<ans) ans <- possible
        }
    }
    ans
}

Primes <- function(n) {
    primes <- c(2)
    while (length(primes) < n) primes <- c(primes, as.integer(nextprime(tail(primes,1))))
    primes
}

Exponents <- function(L) {
    out <- list()
    output <- rep(1,14)
    while (tail(output,1)<L) {
        i<-14
        while ((i > 1) && (output[i]==output[i-1])) {
            output[i]<-1
            i <- i-1
        }
        output[i] <- output[i]+2
        out[[length(out)+1]] <- output
    }
    out
}

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# DiophantineII() 11.85789 11.85789 11.85789 11.85789 11.85789 11.85789     1

a <- DiophantineII()
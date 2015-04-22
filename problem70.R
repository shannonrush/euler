# Euler's Totient function, φ(n) [sometimes called the phi function], is 
# used to determine the number of positive numbers less than or equal to n which are relatively prime to n. 
# For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
# The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
# 
# Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
# 
# Find the value of n, 1 < n < 10^7, 
# for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
options(scipen=999)

TotientPermutation <- function(n) {
    target <- ceiling(sqrt(n))
    primes <- PrimesTo(target+2000)
    primes <- primes[primes>(target-2000)]
    min <- Inf
    min.prod <- 0
    for (a in primes) {
        for (b in primes) {
            prod <- a*b
            if (prod<=n) {
                phi <- (a-1)*(b-1)
                ratio <- prod/phi
                if (ratio < min) {
                    if (PhiPerm(prod,phi)) {
                        min.prod <- prod
                        min <- ratio   
                    }
                }
            }
        }
    }
    min.prod
}

PhiPerm <- function(n,phi) {
    digits <- sort(unlist(strsplit(toString(n),"")))
    phi.digits <- sort(unlist(strsplit(toString(phi),"")))
    identical(digits,phi.digits)
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
# notes
# starting with the first prime under n,
# find the first prime p where φ(p) is a permutation of p

# test
#PhiPerm(87109)==TRUE
#PhiPerm(16)==FALSE

#a <- TotientPermutation(10^7)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# TotientPermutation(10^7) 4.056637 4.056637 4.056637 4.056637 4.056637 4.056637     1
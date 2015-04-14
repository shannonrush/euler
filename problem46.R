# It was proposed by Christian Goldbach that every odd composite number can be written 
# as the sum of a prime and twice a square.
# 
# 9 = 7 + 2×1^2
# 15 = 7 + 2×2^2
# 21 = 3 + 2×3^2
# 25 = 7 + 2×3^2
# 27 = 19 + 2×2^2
# 33 = 31 + 2×1^2
# 
# It turns out that the conjecture was false.
# 
# What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?

GoldbachOther <- function() {
    i <- 3
    while (Goldbach(i)) i <- i+2
    i
}

Goldbach <- function(n) {
    if (IsPrime(n)) return(TRUE)
    p <- 2
    while (p<n) {
        i <- 1
        sum <- p+2*i^2
        while (sum<=n) {
            if (sum==n) return(TRUE)
            i <- i+1
            sum <- p+2*i^2
        }
        p <- NextPrime(p+1)
    }   
    FALSE
}

NextPrime <- function(n) {
    while (!IsPrime(n)) n <- n+1
    n
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m<max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m<-m+1
    }
    TRUE
}

# test
#Goldbach(9)==TRUE
#Goldbach(33)==TRUE
#Goldbach(25)==TRUE
#a<-GoldbachOther()
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# GoldbachOther() 11.39009 11.52632 11.62031 11.63566 11.68583 11.84111    10
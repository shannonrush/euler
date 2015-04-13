# We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once. 
# For example, 2143 is a 4-digit pandigital and is also prime.
# 
# What is the largest n-digit pandigital prime that exists?
library(combinat)

PanPrime <- function() {
    for (n in 9:1) {
        pan.perms <- sort(sapply(permn(1:n), function(x) as.integer(paste(x, collapse=""))), decreasing=T)
        for (pan in pan.perms) if (IsPrime(pan)) return(pan)    
    }
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

# PanPrime()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PanPrime() 11.91496 11.91496 11.91496 11.91496 11.91496 11.91496     1

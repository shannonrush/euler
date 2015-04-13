# The number 3797 has an interesting property. 
# Being prime itself, it is possible to continuously remove digits from left to right, 
# and remain prime at each stage: 3797, 797, 97, and 7. 
# Similarly we can work from right to left: 3797, 379, 37, and 3.
# 
# Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
# 
# NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

TruncPrimes <- function() {
    t <- c()
    n <- 8
    while (length(t)<11) {
       if (IsTrunctable(n)) t <- c(t, n)   
       n <- n+1
    }
    sum(t)
}

IsTrunctable <- function(n) {
    if (!IsPrime(n)) return(FALSE)
    digits <- unlist(strsplit(toString(n),""))
    for (i in (length(digits)-1):1) if (!IsPrime(as.integer(paste(tail(digits,i),collapse="")))) return(FALSE)
    for (j in (length(digits)-1):1) if (!IsPrime(as.integer(paste(head(digits,j),collapse="")))) return(FALSE)
    TRUE
}

IsPrime <- function(n) {
    if (n==1) return(FALSE)
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
#IsTrunctable(3797)==TRUE

# TruncPrimes()

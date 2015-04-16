# The primes 3, 7, 109, and 673, are quite remarkable. 
# By taking any two primes and concatenating them in any order the result will always be prime. 
# For example, taking 7 and 109, both 7109 and 1097 are prime. 
# The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
# 
# Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

PrimeSets <- function(n) {
    primes <- PrimesTo(10000)
    set <- c()
    while (length(set)!=n) {
        set <- c(primes[1])
        primes <- primes[-1]
        set <- MakeSet(n, set, primes)
    }
    sum(set)
}

MakeSet <- function(n, set, primes) {
    print(set)
    if (length(set)==n) {
        return(set)
    } else { 
        for (p in primes) {
            if (p > tail(set, 1) && AllPrimes(set, p)) {
                new.set <- MakeSet(n, c(set,p), primes)
                if (length(new.set)>0) return(new.set)
            }
        }
        return(c())
    }
}

AllPrimes <- function(set, k) {
    for (p in set) {
        if (!IsPrime(as.numeric(paste0(p,k,collapse="")))) return(FALSE) 
        if (!IsPrime(as.numeric(paste0(k,p,collapse="")))) return(FALSE)    
    }
    TRUE
}

PrimesTo <- function(n) {
    which(sapply(2:n, IsPrime))+1
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
#PrimeSets(4)==792
#AllPrimes(c(3, 7, 109),673)==TRUE
#AllPrimes(c(3, 7, 109),929)==FALSE

#a <- PrimeSets(5)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PrimeSets(5) 232.7264 232.7264 232.7264 232.7264 232.7264 232.7264     1
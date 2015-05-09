library(gtools)
library(gmp)

PrimeRuns <- function(n) {
    filler <- sapply(1:2, function(x) permutations(10, x, 0:9, repeats.allowed=T))
    M <- rep(0,10)
    S <- rep(0,10)
    for (i in 1:10) M[i] <- MaxRepeating(i-1, filler, n)
    for (i in 1:10) S[i] <- SumRepeating(i-1, M[i], n, filler[[n-M[i]]]) # i is repeating digit, M[i] is # repeating
    sum <- as.bigz(0)
    for (s in S) sum <- sum+as.bigz(s)
    sum
}

SumRepeating <- function(i, m, n, f) {
    # find all primes of nchar n with m repeating i's
    sum <- as.bigz(0)
    filler.i <- combinations(n, n-m, 1:n) # which spaces to fill
    for (k in 1:nrow(f)) {
        fill <- f[k,] # what to fill spaces with
        if (!any(fill==i)) {
            for (l in 1:nrow(filler.i)) {
                f.i <- filler.i[l,] ## 
                digits <- rep(i,n)
                digits[f.i] <- fill
                if (digits[1]!=0) {
                    number <- as.bigz(paste(digits,collapse=""))
                    if (isprime(number)>0) sum <- sum+number
                }
            }
        }
    }
    as.character(sum)
}

MaxRepeating <- function(i, filler, n) {
    for (j in n:2) { # number of times repeated
        # is there a prime number with i repeating j times?
        if (j!=n) {
            f <- filler[[n-j]]
            filler.i <- combinations(n, n-j, 1:n) # places to put filler
            for (k in 1:nrow(f)) {
                fill <- f[k,]
                # can't have any i's
                if (!any(fill==i)) {
                    for (l in 1:nrow(filler.i)) {
                        f.i <- filler.i[l,]
                        digits <- rep(i,n)
                        digits[f.i] <- fill
                        if (digits[1]!=0) {
                            number <- as.bigz(paste(digits,collapse=""))
                            if (isprime(number)>0) return(j)
                        }
                    }
                }
            }
        }
    }
}

# test
#PrimeRuns(4)==273700
# a <- PrimeRuns(10)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# PrimeRuns(10) 338.0396 339.6996 356.8668 342.9078 360.9969 420.0061    10


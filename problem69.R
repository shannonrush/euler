# Euler's Totient function, φ(n) [sometimes called the phi function], 
# is used to determine the number of numbers less than n which are relatively prime to n. 
# For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
# 
#                 n	    Relatively Prime	φ(n)	n/φ(n)
#                 
#                 2	    1	                1	    2
#                 3	    1,2	                2	    1.5
#                 4	    1,3	                2	    2
#                 5	    1,2,3,4	            4	    1.25
#                 6	    1,5	                2       3
#                 7	    1,2,3,4,5,6	        6	    1.1666...
#                 8	    1,3,5,7	            4	    2
#                 9	    1,2,4,5,7,8	        6   	1.5
#                 10	1,3,7,9	            4   	2.5
# 
# It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
# 
# Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

TotientMax <- function(n) {
    primes <- PrimesTo(100)
    prods <- Reduce(prod, primes, accumulate=T)
    max(prods[prods<n])
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
#TotientMax(10)==6
#a <- TotientMax(1000000)

# Unit: microseconds
# expr     min      lq     mean   median      uq     max neval
# TotientMax(1000000) 508.746 533.145 552.7715 546.2015 568.542 683.352   100
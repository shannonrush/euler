# A unit fraction contains 1 in the numerator. 
# The decimal representation of the unit fractions with denominators 2 to 10 are given:
#     
# 1/2    = 	0.5
# 1/3	= 	0.(3)
# 1/4	= 	0.25
# 1/5	= 	0.2
# 1/6	= 	0.1(6)
# 1/7	= 	0.(142857)
# 1/8	= 	0.125
# 1/9	= 	0.(1)
# 1/10	= 	0.1
# Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
# 
# Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

library(gmp)

RecipCycles <- function(n) {
    for (p in n:2) if (IsPrime(p) && FullReptend(p)) return(p)
}

FullReptend <- function(p) {
    a <- pow.bigz(as.bigz(10),p-1)
    b <- mod.bigz(a,p)
    if (b != 1) return(FALSE)
    for (k in 1:(p-2)) {
        x <- pow.bigz(as.bigz(10),k)
        y <- mod.bigz(x,p)
        if (y==1) return(FALSE)
    }
    TRUE
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

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# RecipCycles(999) 53.35506 54.33584 55.29898 55.13303 55.80336 61.13653   100


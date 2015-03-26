# A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. 
# For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
# 
# A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if 
# this sum exceeds n.
# 
# As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as 
# the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 
# 28123 can be written as the sum of two abundant numbers. 
# However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number 
# that cannot be expressed as the sum of two abundant numbers is less than this limit.
# 
# Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.

NonAbundantSums <- function() {
    abundant <- AbundantNumbers()
    sum <- 0
    for (i in 1:28123) {
        chk <- i-abundant
        if(!any(chk %in% abundant)) sum <- sum+i
    }
    sum
}

AbundantNumbers <- function() {
    a <- c()
    n <- 1
    while (n<=28123) {
        if (n<d(n)) a <- c(a,n)
        n <- n+1
    }
    a
}

d <- function(n) {
    m <- 2
    max <- n
    divs <- c(1)
    while (m<max) {
        if (n%%m==0) {
            divs <- c(divs, m, (n/m))
        }
        max <- ceiling(n/m)
        m<-m+1
    }
    sum(unique(divs))
}

# test
#d(12)==16
#d(4)==3

#NonAbundantSums()
# 4179871

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# NonAbundantSums() 19.48285 19.49584 19.65546 19.58224 19.80044 19.99961    10


# Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
# If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
# 
# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
# The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
# 
# Evaluate the sum of all the amicable numbers under 10000.

AmicableNumbers <- function() {
    sum <- 0
    n<-1
    checked <- c()
    while (n<10000) {
        if (!(n %in% checked)) {
            b <- d(n)
            if (n!=b && d(b)==n) sum <- sum+n+b
            checked <- c(checked, n, b)
        }
        n <- n+1
    }
    sum
}

d <- function(n) {
    m <- 2
    max <- n
    sum <- 1
    while (m<max) {
        if (n%%m==0) sum <- sum + m + (n/m)
        max <- ceiling(n/m)
        m<-m+1
    }
    sum
}

# test
# d(220)==284
# d(284)==220
# 
# answer <- AmicableNumbers()
# 31626

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# AmicableNumbers() 4.356886 4.514841 4.594631 4.561669 4.637441 5.146544   100
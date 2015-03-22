# The following iterative sequence is defined for the set of positive integers:
# 
# n → n/2 (n is even)
# n → 3n + 1 (n is odd)
# 
# Using the rule above and starting with 13, we generate the following sequence:
#     
#     13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
# It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
# Although it has not been proved yet (Collatz Problem), 
# it is thought that all starting numbers finish at 1.
# 
# Which starting number, under one million, produces the longest chain?
# 
# NOTE: Once the chain starts the terms are allowed to go above one million.

LongestCollatz <- function(n) {
    sequences <- c()
    max <- 0
    longest.starter <- 0
    for (i in n:1) {
        print(i)
        if (!(i %in% sequences)) {
            next.seq <- NextCollatz(i, c(i))
            sequences <- unique(c(sequences, next.seq))
            if (length(next.seq)>max) {
                max <- length(next.seq)
                longest.starter<-i
            }
        }
    }
    longest.starter
}

NextCollatz <- function(n, seq) {
    if (n==1) {
        return(seq)
    } else {
        n <- ifelse(n%%2==0, n/2, 3*n+1)
        seq <- c(seq, n)
        NextCollatz(n, seq)
    }
}

# test
identical(NextCollatz(13, c(13)), c(13,40,20,10,5,16,8,4,2,1))
LongestCollatz(13)==9

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# LongestCollatz(1000) 232.2542 240.6403 248.6847 245.5917 252.3264 311.9296   100

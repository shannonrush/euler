# It is possible to show that the square root of two can be expressed as an infinite continued fraction.
# 
# √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
# 
# By expanding this for the first four iterations, we get:
#     
# 1 + 1/2 = 3/2 = 1.5
# 1 + 1/(2 + 1/2) = 7/5 = 1.4
# 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
# 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
# 
# The next three expansions are 99/70, 239/169, and 577/408, 
# but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator 
# exceeds the number of digits in the denominator.
# 
# In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
library(gmp)

SqrtConv <- function() {
    count <- 0
    n <- as.bigz(3)
    d <- as.bigz(2)
    for (i in 2:1000) {
        if (nchar(as.character(n))>nchar(as.character(d))) count <- count+1
        new.d <- n+d
        n <- 2*d+n
        d <- new.d
    }
    count
}

# a<-SqrtConv()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# SqrtConv() 58.28372 59.52256 60.80981 60.56177 61.43559 73.00663   100
# It is well known that if the square root of a natural number is not an integer, then it is irrational. 
# The decimal expansion of such square roots is infinite without any repeating pattern at all.
# 
# The square root of two is 1.41421356237309504880..., 
# and the digital sum of the first one hundred decimal digits is 475.
# 
# For the first one hundred natural numbers, 
# find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.
library(gmp)
options(scipen=999)

SqrtExpansion <- function() {
    sum(sapply(1:100, SumDigits))
}

SumDigits <- function(n) {
    if (sqrt(n)%%1==0) return(0)
    a <- as.bigz(5*n)
    b <- as.bigz(5)
    while (nchar(as.character(b))<150) {
        if (a >= b) {
            a <- a-b
            b <- b+10
        } else {
            a <- a * 100
            b.digits <- unlist(strsplit(toString(b),""))
            b <- as.bigz(paste(append(b.digits,"0",length(b.digits)-1),collapse=""))
        }
    }
    sum(as.integer(unlist(strsplit(toString(b),"")))[1:100])
}

# test
# SumDigits(2)==475
# a <- SqrtExpansion()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# SqrtExpansion() 4.229571 4.229571 4.229571 4.229571 4.229571 4.229571     1

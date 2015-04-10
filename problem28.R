# Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is 
# formed as follows:
 
# 21 22 23 24 25 
# 20  7  8  9 10
# 19  6  1  2 11
# 18  5  4  3 12
# 17 16 15 14 13

# It can be verified that the sum of the numbers on the diagonals is 101.
# 
# What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

SpiralDiag <- function(n) {
    sum <- 1
    p <- 1
    by <- 2
    start <- 1
    for (i in 1:floor(n/2)) {
        start <- start + (2*i)
        end <- start+6*i
        sum <- sum+sum(seq(start,end,by=by))
        by <- by+2
        start <- end
    }
    sum
}

# test
# SpiralDiag(5)==101

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# SpiralDiag(1001) 13.13936 13.81984 14.25422 14.05193 14.77044 18.05035   100

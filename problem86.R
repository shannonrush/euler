# A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite corner. 
# By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10.
# 
# However, there are up to three "shortest" path candidates for any given cuboid and the shortest route doesn't always have integer length.
# 
# It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer dimensions, up to a maximum size of M by M by M, 
# for which the shortest route has integer length when M = 100. 
# This is the least value of M for which the number of solutions first exceeds two thousand; the number of solutions when M = 99 is 1975.
# 
# Find the least value of M such that the number of solutions first exceeds one million.

library(Rcpp)
sourceCpp('problem86.cpp')

CuboidRoute <- function(n) {
    m <- 1
    count <- 0
    while (count<n) {
        count <- count + CountRoutes(m)
        m <- m+1
    } 
    m-1
}

# test
#ShortestRoute(6,5,3)==10
#CuboidRoute(2000)==100
#a <- CuboidRoute(10^6)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# CuboidRoute(10^6) 28.35629 28.35629 28.35629 28.35629 28.35629 28.35629     1


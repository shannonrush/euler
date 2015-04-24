# Let p(n) represent the number of different ways in which n coins can be separated into piles. 
# For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.
# 
# Find the least value of n for which p(n) is divisible by one million.

options(scipen=999)
library(gmp)
CoinPartitions <- function() {
    k <- c(sapply(1:1000, function(x) c(Pent(x),Pent(-x))))
    p <- c(as.bigz(c(1,2)))
    m<-10^6
    n <- 3
    while (tail(p,1)%%m!=0) {
        print(n)
        sum <- 0
        i <- 1
        while (n>=k[i]) {
             sign <- ifelse(i%%4 %in% c(1,2), 1, -1)
             if (n==k[i]) {
                 sum <- sum + 1 * sign
             } else {
                 sum <- sum + p[n-k[i]] * sign   
             }
             i <- i+1
         }
         p <- c(p, sum)
         n <- n+1
    }
    n-1
}

Pent <- function(n) (n*(3*n-1))/2

a <- CoinPartitions()

# note
# this works, however it takes a very, very long time. 
# I attempted to write an R/cpp combo solution but as of now I have been unable to come up with a way for Rcpp to work nicely with
# GMP so my ultimate solution for now is straight cpp (in problem78cpp.cpp named in this crazy way to distinguish it from problem78.cpp,
# which is where the R/cpp solution will someday live) which successfully completes in well under a second.





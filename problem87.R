# The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. 
# In fact, there are exactly four numbers below fifty that can be expressed in such a way:
#     
# 28 = 2^2 + 2^3 + 2^4
# 33 = 3^2 + 2^3 + 2^4
# 49 = 5^2 + 2^3 + 2^4
# 47 = 2^2 + 3^3 + 2^4
# 
# How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
library(Rcpp)
sourceCpp('problem87.cpp')

PrimePowerTriples <- function(n) {
    length(sort(CountTriples(PrimesTo(sqrt(n)),n)))
}

PrimesTo <- function(n) {
    which(sapply(2:n,IsPrime))+1
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m<max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m<-m+1
    }
    TRUE
}

#test

#PrimePowerTriples(50)==4
#a <- PrimePowerTriples(50000000)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PrimePowerTriples(5e+07) 1.339763 1.339763 1.339763 1.339763 1.339763 1.339763     1

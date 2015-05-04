# The first known prime found to exceed one million digits was discovered in 1999, 
# and is a Mersenne prime of the form 2^(6972593)−1; 
# 
# it contains exactly 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^(p)−1, 
# have been found which contain more digits.
# 
# However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433×2^(7830457)+1.
# 
# Find the last ten digits of this prime number.

library(gmp)

NonMersenne <- function() {
    a <- as.character(as.bigz(28433) * as.bigz(2)^7830457 + 1)
    substr(a,nchar(a)-9,nchar(a))
}

# a <- NonMersenne()
# 
# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# NonMersenne() 439.9936 445.1875 454.1278 452.6553 460.3108 474.5151    10
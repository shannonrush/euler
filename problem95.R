# The proper divisors of a number are all the divisors excluding the number itself. 
# For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. 
# 
# As the sum of these divisors is equal to 28, we call it a perfect number.
# 
# Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, 
# forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.
# 
# Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:
#     
#     12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
# 
# Since this chain returns to its starting point, it is called an amicable chain.
# 
# Find the smallest member of the longest amicable chain with no element exceeding one million.

library(gmp)

AmicableChain <- function() {
    max.chain <- 0
    min.value <- Inf
    for (n in seq(4,999999,2)) {
        print(n)
        chain <- Chain(n)
        if (length(chain)>max.chain) {
            max.chain <- length(chain)
            min.value <- min(chain)
        }
    }
    min.value
}

Chain <- function(n) {
    sum <- n
    chain <- c(sum)
    while (sum<10^6) {
        sum <- SumProperDiv(sum)
        if (sum %in% c(1,0)) return(c())
        if (sum==n) return(chain)
        if (sum>10^6) return(c())
        if (sum %in% chain) return(c())
        chain <- c(chain, sum)
    }
}

SumProperDiv <- function(sum) {
    f <- as.integer(factorize(sum))
    prod(sapply(unique(f), function(p) (p^(sum(f==p)+1)-1)/(p-1)))-sum
}

a <- AmicableChain()
# test
# SumProperDiv(28)==28
# SumProperDiv(220)==284
# SumProperDiv(12)==16

# notes
# aka "Aliquot Sequence"
#
# table of 1-275 http://oeis.org/A044050/b044050.txt
#
# Sequences starting at even numbers are, as a rule, longer than sequences starting at odd numbers
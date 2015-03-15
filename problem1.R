# If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
# The sum of these multiples is 23.
# 
# Find the sum of all the multiples of 3 or 5 below 1000.

SumMultiples <- function(n) {
    sum(which(1:n%%3==0 | 1:n%%5==0))
}

SumMultiples(999)
# [1] 233168

# O(n)



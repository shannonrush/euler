# The square of the sum of the first ten natural numbers is,
# 
# (1 + 2 + ... + 10)^2 = 55^2 = 3025
# 
# The sum of the squares of the first ten natural numbers is,
# 
# 1^2 + 2^2 + ... + 10^2 = 385
# 
# Hence the difference between the sum of the squares of the first ten natural numbers 
# and the square of the sum is 3025 − 385 = 2640.
# 
# Find the difference between the sum of the squares of the first one hundred natural numbers 
# and the square of the sum.

SumSquareDiff <- function(n) {
    sum(1:n)^2-sum(sapply(1:n, function(i) i^2))     
}

# test
SumSquareDiff(10)==2640

SumSquareDiff(100)
# [1] 25164150

# Unit: microseconds
# expr     min       lq      mean   median      uq     max neval
# SumSquareDiff(10)  37.598  40.9520  45.40818  42.5095  45.011 173.861   100
# SumSquareDiff(100) 143.669 152.6505 159.96335 159.3335 165.756 183.639   100

# O(n)
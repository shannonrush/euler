# n! means n × (n − 1) × ... × 3 × 2 × 1
# 
# For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
# and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
# 
# Find the sum of the digits in the number 100!

FactDigitSum <- function(n) {
    library(gmp)
    sum(as.numeric(unlist(strsplit(toString(factorialZ(n)),""))))
}

# test
#FactDigitSum(10)==27

#FactDigitSum(100)
# 648

# Unit: microseconds
# expr     min      lq     mean   median      uq     max neval
# FactDigitSum(100) 178.918 183.819 192.4706 187.2925 197.649 378.604   100


# 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
# 
# Find the sum of all numbers which are equal to the sum of the factorial of their digits.
# 
# Note: as 1! = 1 and 2! = 2 are not sums they are not included.

DigitFactorials <- function() {
    sum <- 0
    for (i in 3:99999) if (SumFacts(i)) sum <- sum + i
    sum
}

SumFacts <- function(n) {
    digits <- as.integer(unlist(strsplit(toString(n),"")))
    ifelse(sum(factorial(digits))==n, TRUE, FALSE)
}

# test
# SumFacts(145)==TRUE

# DigitFactorials()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# DigitFactorials() 2.184715 2.217862 2.252604 2.241981 2.303971 2.319154    10
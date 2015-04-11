# Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
#     
# 1634 = 14 + 64 + 34 + 44
# 8208 = 84 + 24 + 04 + 84
# 9474 = 94 + 44 + 74 + 44
# 
# As 1 = 1^4 is not a sum it is not included.
# 
# The sum of these numbers is 1634 + 8208 + 9474 = 19316.
# 
# Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

DigitPowers <- function(n) {
    success <- c()
    end <- 9^n*(n+1)
    for (i in 2:end) {
        digits <- as.integer(unlist(strsplit(as.character(i),"")))
        if (sum(digits^n)==i) success <- c(success, i)
    }
    sum(success)
}

# test
#DigitPowers(4)==19316

#DigitPowers(5)

# Unit: seconds
# expr      min      lq     mean  median       uq     max neval
# DigitPowers(5) 2.619721 2.80714 2.833681 2.85753 2.902232 2.93632    10
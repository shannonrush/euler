# A googol (10^100) is a massive number: one followed by one-hundred zeros; 
# 100^100 is almost unimaginably large: one followed by two-hundred zeros. 
# 
# Despite their size, the sum of the digits in each number is only 1.
# 
# Considering natural numbers of the form, a^b, where a, b < 100, what is the maximum digital sum?

library(gmp)

PowerDigitSum <- function() {
    max(sapply(1:99, function(a) sapply(1:99, function(b) sum(as.numeric(unlist(strsplit(toString(as.bigz(a)^b),"")))))))
}

# a <- PowerDigitSum()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# PowerDigitSum() 611.6068 617.9712 625.1786 622.3513 628.9981 661.7838   100
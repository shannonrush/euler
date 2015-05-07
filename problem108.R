# In the following equation x, y, and n are positive integers.
# 1/x + 1/y = 1/n
# 
# For n = 4 there are exactly three distinct solutions:
# 1/5 + 1/20 = 1/4
# 1/6 + 1/12 = 1/4
# 1/8 + 1/8 = 1/4
# 
# What is the least value of n for which the number of distinct solutions exceeds one-thousand?
library(gmp)

DiophantineRecip <- function() {
    i <- 2
    while (NumFactors(i^2)<2000) i <- i+1
    i
}

NumFactors <- function(n) {
    factors <- as.integer(factorize(n))
    prod(table(factors)+1)
}

#a <- DiophantineRecip()

# notes
# NumSolutions <- function(n) (NumFactors(n^2)+1)/2
# so we just need to find the n whose square results in >= 2000 factors

# Unit: seconds
# expr     min      lq    mean  median      uq     max neval
# DiophantineRecip() 42.9843 42.9843 42.9843 42.9843 42.9843 42.9843     1

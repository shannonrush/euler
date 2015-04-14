# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
# 
# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

library(gmp)

SelfPowers <- function(n) {
    sum <- 0
    for (i in 1:n) sum <- sum + as.bigz(i)^i
    sum <- as.character(sum)
    substr(sum, nchar(sum)-9, nchar(sum))
}

# test
#SelfPowers(10)=="0405071317"

#SelfPowers(1000)

# Unit: microseconds
# expr       min        lq       mean     median        uq      max neval
# SelfPowers(10)   291.091   302.554   346.5679   312.2365   327.794  2749.95   100
# SelfPowers(1000) 38564.473 39746.998 41026.5630 40405.4590 42176.196 49768.76   100
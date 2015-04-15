# There are exactly ten ways of selecting three from five, 12345:
#     
#     123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
# 
# In combinatorics, we use the notation, 5C3 = 10.
# 
# It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
# 
# How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?

CombSelect <- function() {
    values <- 0
    for (n in 1:100) {
        for (r in 1:n) if (choose(n,r) > 1000000) values <- values + 1
    }
    values
}

#a <- CombSelect()

# Unit: milliseconds
# expr      min      lq     mean   median      uq      max neval
# CombSelect() 5.520526 5.71106 6.490678 6.284366 6.98017 10.57333   100
# It is possible to write five as a sum in exactly six different ways:
#     
# 4 + 1
# 3 + 2
# 3 + 1 + 1
# 2 + 2 + 1
# 2 + 1 + 1 + 1
# 1 + 1 + 1 + 1 + 1
# 
# How many different ways can one hundred be written as a sum of at least two positive integers?

options(scipen=999)

CountSums <- function(n) {
    combos <- rep(0,n)
    for (i in 1:(n-1)) {
        for (j in i:n) {
            a <- ifelse(j-i==0,1,combos[j-i])
            combos[j] <- combos[j]+a
        }
    }
    combos[n]
}

# test
# CountSums(5)==6

# a <- CountSums(100)

# Unit: milliseconds
# expr      min       lq     mean  median       uq     max neval
# CountSums(100) 21.82504 22.83801 23.12964 23.1403 23.40845 24.4138    10

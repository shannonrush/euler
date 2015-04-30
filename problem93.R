# By using each of the digits from the set, {1, 2, 3, 4}, exactly once, 
# and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses, 
# it is possible to form different positive integer targets.
# 
# For example,
# 
# 8 = (4 * (1 + 3)) / 2
# 14 = 4 * (3 + 1 / 2)
# 19 = 4 * (2 + 3) − 1
# 36 = 3 * 4 * (2 + 1)
# 
# Note that concatenations of the digits, like 12 + 34, are not allowed.
# 
# Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, 
# and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.
# 
# Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 
# 1 to n, can be obtained, giving your answer as a string: abcd.
library(combinat)

Arithmetic <- function() {
    ops <- Ops()
    ops.perms <- OpsPerms()
    combos <- t(combn(1:9,4))
    consec <- apply(combos, 1, Consecutive, ops.perms, ops)
    combos[which.max(consec),]
}

Consecutive <- function(combo, ops.perms, ops) {
    perms <- permn(combo)
    results <- c()
    # for every perm apply every op.perms
    for (i in 1:length(perms)) {
        set <- perms[i][[1]]
        for (j in 1:nrow(ops.perms)) {
            op <- ops[ops.perms[j,]]
            result1 <- op[1][[1]](op[2][[1]](set[1],set[2]),op[3][[1]](set[3],set[4]))
            if (result1 > 0 && result1%%1==0) results <- c(results, result1)
            result2 <- op[1][[1]](op[2][[1]](op[3][[1]](set[1],set[2]),set[3]),set[4]) 
            if (result2 > 0 && result2%%1==0) results <- c(results, result2)
        }
    }
    i <- 1
    while (any(results==i)) i <- i+1
    i-1
}

Ops <- function() {
    add <- match.fun("+")
    sub <- match.fun("-")
    mult <- match.fun("*")
    div <- match.fun("/")
    c(add, sub, mult, div)
}

OpsPerms <- function() {
    ops.combs <- t(combn(1:4,3))
    ops.perms <- matrix(ncol=3)
    for (i in 1:nrow(ops.combs)) {
        o <- ops.combs[i,]
        e <- as.matrix(expand.grid(rep(list(o),3)))
        ops.perms <- rbind(ops.perms, e)    
    }
    ops.perms[-1,]
}

# test
# Consecutive(c(1,2,3,4),OpsPerms(),Ops())==28
#a <- Arithmetic()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# Arithmetic() 4.782482 4.782482 4.782482 4.782482 4.782482 4.782482     1

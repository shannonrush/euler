# It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, 
# but in a different order.
# 
# Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.

PermMult <- function() {
    i <- 1
    while (!Permutable(i)) i <- i+1
    i
}

Permutable <- function(n) {
    digits <- sort(unlist(strsplit(toString(n),"")))
    for (i in 2:6) {
        if (!identical(sort(unlist(strsplit(toString(n*i),""))), digits)) return(FALSE)
    }
    TRUE
}

# a <- PermMult()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PermMult() 10.66385 10.66385 10.66385 10.66385 10.66385 10.66385     1

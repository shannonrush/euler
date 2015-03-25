# By starting at the top of the triangle below and moving to adjacent numbers on the row below, 
# the maximum total from top to bottom is 23.
# 
# 3
# 7 4
# 2 4 6
# 8 5 9 3
# 
# That is, 3 + 7 + 4 + 9 = 23.
# 
# Find the maximum total from top to bottom in triangle.txt, a 15K text file containing a triangle with one-hundred rows.
# 
# NOTE: This is a much more difficult version of Problem 18. It is not possible to try every route to solve this problem, 
# as there are 2^99 altogether! If you could check one trillion (10^12) routes every second it would take over twenty billion 
# years to check them all. There is an efficient algorithm to solve it. ;o)

MaxPathSum <- function() {
    M <- GetM() 
    ReduceTriangle(M)    
}

ReduceTriangle <- function(M) {
    i <- length(M)-1
    while (i>=1) {
        for (j in 1:length(M[[i]])) {
            x <- M[[i]][j]
            M[[i]][j] <- x + max(M[[i+1]][j], M[[i+1]][j+1])
        }
        i <- i-1
    }
    M[[1]][1]
}

GetM <- function() {
    s <- readLines("resources/p067_triangle.txt")
    lapply(strsplit(as.matrix(s)," "), as.integer)
}

# answer <- MaxPathSum()
# 7273

# Unit: milliseconds
# expr      min       lq     mean  median       uq      max neval
# MaxPathSum() 31.24229 34.28964 35.99904 35.2683 36.26173 92.11434   100

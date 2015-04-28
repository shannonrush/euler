# The minimal path sum in the 5 by 5 matrix below, 
# by starting in any cell in the left column and finishing in any cell in the right column, 
# and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.
# 
# Find the minimal path sum, in matrix.txt, a 31K text file containing a 80 by 80 matrix, 
# from the left column to the right column.

PathSum <- function() {
    M <- unname(as.matrix(read.table("resources/p082_matrix.txt",sep=",")))
    size <- nrow(M)
    # cost is a vector representing the minimum cost to get to each row of the target column
    cost <- M[,size] # initialize cost to target column
    for (i in (size-1):1) { # for each row from second from bottom to top
        cost[1] <- cost[1]+M[1,i] # cost of going right
        for (j in 1:size) cost[j] <- min(cost[j-1]+M[j,i], cost[j]+M[j,i]) # does it cost more to go right or up/right?
        for (j in (size-1):1) cost[j] <- min(cost[j], cost[j+1]+M[j,i]) # does it cost more to go down or the previous choice?
    }
    min(cost)
}

# a <- PathSum()

# Unit: milliseconds
# expr      min       lq     mean   median      uq      max neval
# PathSum() 48.09748 49.24536 51.50821 49.98086 51.3426 89.88365   100
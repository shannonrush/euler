# In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, 
# by moving left, right, up, and down, is indicated in bold red and is equal to 2297.
# 
# Find the minimal path sum, a 31K text file containing a 80 by 80 matrix, 
# from the top left to the bottom right by moving left, right, up, and down.

PathSum <- function() {
    M <- unname(as.matrix(read.table("resources/p083_matrix.txt",sep=",")))
    size <- nrow(M)
    cost <- matrix(Inf, nrow=size, ncol=size)
    cost[1,1] <- M[1,1]
    open <- data.frame(cost=cost[1,1], i=1, j=1)
    
    while (cost[size,size]==Inf) {
        open <- open[with(open, order(cost)), ]
        current.i <- open[1, "i"]
        current.j <- open[1, "j"]
        open <- open[-1,]
        neighbors <- Neighbors(current.i, current.j, size)
        for (n in 1:length(neighbors)) {
            neighbor <- neighbors[[n]]
            i <- neighbor[1]
            j <- neighbor[2]
            if (cost[i,j] > cost[current.i, current.j]+M[i,j]) {
                cost[i,j] <- cost[current.i,current.j]+M[i,j]
                open <- rbind(open, data.frame(cost=cost[i,j],i=i,j=j))
            }
        }
    }
    cost[size,size]
}

Neighbors <- function(i, j, size) {
    n <- list()
    if (i!=1) n[[length(n)+1]] <- c(i-1,j)
    if (i!=size) n[[length(n)+1]] <- c(i+1,j)
    if (j!=1) n[[length(n)+1]] <- c(i,j-1)
    if (j!=size) n[[length(n)+1]] <- c(i,j+1)
    n
}

#a <- PathSum()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PathSum() 5.372859 5.372859 5.372859 5.372859 5.372859 5.372859     1
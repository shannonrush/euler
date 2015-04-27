# Find the minimal path sum, in matrix.txt a 31K text file containing a 80 by 80 matrix, 
# from the top left to the bottom right by only moving right and down.

PathSum <- function() {
    M <- as.matrix(read.table("resources/p081_matrix.txt",sep=","))
    size <- nrow(M)
    M[size,] <- Reduce(sum, M[size,], accumulate=T, right=T)
    M[,size] <- Reduce(sum, M[,size], accumulate=T, right=T)
    for (i in (size-1):1) for (j in (size-1):1) M[i,j] <- M[i,j] + min(M[i+1,j], M[i, j+1])
    M[1,1]
}

# a <- PathSum()
# 
# Unit: milliseconds
# expr     min       lq     mean   median       uq      max neval
# PathSum() 49.7062 51.25253 53.16599 51.87685 52.64761 99.24024   100
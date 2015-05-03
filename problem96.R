# Example at: https://projecteuler.net/problem=96
# 
# A well constructed Su Doku puzzle has a unique solution and can be solved by logic, 
# although it may be necessary to employ "guess and test" methods in order to eliminate options 
# (there is much contested opinion over this). 
# 
# The complexity of the search determines the difficulty of the puzzle; 
# the example above is considered easy because it can be solved by straight forward direct deduction.
# 
# The 6K text file sudoku.txt contains fifty different Su Doku puzzles ranging in difficulty, 
# but all with unique solutions.
# 
# By solving all fifty puzzles find the sum of the 3-digit numbers found in the top left corner of each solution grid; 
# for example, 483 is the 3-digit number found in the top left corner of the solution grid above.

Sudoku <- function() {
    sum <- 0
    lines <- readLines("resources/p096_sudoku.txt")
    while (length(lines>0)) {
        grid <- lines[2:10]
        lines <- lines[-(1:10)]
        sum <- sum + Solve(grid)
    }
    sum
}

Solve <- function(grid) {
    m <- GridToMatrix(grid)
    orig.m <- m
    err <- sum(m==0)
    while (err!=0) {
        m <- FillNext(m)
        if (sum(m==0)==err) m <- orig.m
        err <- sum(m==0)
    }
    as.integer(paste0(m[1,1:3],collapse=""))
}

FillNext <- function(m) {
    for (n in 1:8) {
        for (i in 1:9) {
            if (sum(m[i,]==0)==n) return(FillRow(m,i))
            if (sum(m[,i]==0)==n) return(FillCol(m,i))
        }
        for (i in seq(1,9,3)) {
            for (j in seq(1,9,3)) {
                if (sum(m[i:(i+2),j:(j+2)]==0)==n) {
                    return(FillGrid(m, i, j))
                }
            }
        }
    }
}

Valid <- function(m) {
    for (i in 1:9) {
        if (any(duplicated(m[i,],incomparables=0))) return(FALSE)
        if (any(duplicated(m[,i],incomparables=0))) return(FALSE)
        for (i in seq(1,9,3)) {
            for (j in seq(1,9,3)) {
                if (any(duplicated(as.integer(m[i:(i+2),j:(j+2)]),incomparables=0))) return(FALSE) 
            }
        }
    }
    TRUE
}

FillRow <- function(m,i) {
    orig.m <- m
    diff <- setdiff(1:9, m[i,])
    to.fill <- m[i,]==0
    if (length(diff)==1) {
        m[i,to.fill] <- diff
        if (!Valid(m)) {
            return(orig.m) 
        } else {
            return(m)
        }
    }
    m[i,to.fill] <- sample(diff)
    count <- 0
    while (!Valid(m)) {
        if (count>100) return(orig.m)
        m[i,to.fill] <- sample(diff)
        count <- count+1
    }
    m
}

FillCol <- function(m,i) {
    orig.m <- m
    diff <- setdiff(1:9, m[,i])
    to.fill <- m[,i]==0
    if (length(diff)==1) {
        m[to.fill,i] <- diff
        if (!Valid(m)) {
            return(orig.m) 
        } else {
            return(m)
        }
    }
    m[to.fill,i] <- sample(diff)
    count <- 0
    while (!Valid(m)) {
        if (count>100) return(orig.m)
        m[to.fill,i] <- sample(diff)
        count <- count+1
    }
    m
}

FillGrid <- function(m,i,j) {
    orig.m <- m
    grid <- as.integer(m[i:(i+2),j:(j+2)])
    diff <- setdiff(1:9, grid)
    to.fill <- grid==0
    if (length(diff)==1) {
        grid[to.fill] <- diff
        m[i:(i+2),j:(j+2)] <- grid
        if (!Valid(m)) {
            return(orig.m) 
        } else {
            return(m)
        }
    }
    grid[to.fill] <- sample(diff)
    m[i:(i+2),j:(j+2)] <- grid
    count <- 0
    while (!Valid(m)) {
        if (count>100) return(orig.m)
        grid[to.fill] <- sample(diff)
        m[i:(i+2),j:(j+2)] <- grid
        count <- count+1
    }
    m
}

GridToMatrix <- function(grid) {
    unname(t(sapply(grid, function(x) as.integer(unlist(strsplit(x,""))))))
}

# test
#grid <- c("003020600","900305001","001806400","008102900","700000008","006708200","002609500","800203009","005010300")
#Solve(grid)==15

#a <- Sudoku()

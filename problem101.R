OptimumPoly <- function() {
    gens <- Generate(10)
    diffs <- gens 
    sum <- 0
    while (length(diffs)>1) {
        sum <- sum+sum(diffs)
        diffs <- rev(sapply(length(diffs):2, function(x) diffs[x]-diffs[x-1]))
    }
    sum+sum(diffs)
}

Generate <- function(x) {
    sapply(1:x, function(n) 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10)
}

# a <- OptimumPoly()

# Unit: microseconds
# expr     min      lq     mean   median      uq     max neval
# OptimumPoly() 442.663 464.412 490.1524 481.9705 499.695 605.329    10

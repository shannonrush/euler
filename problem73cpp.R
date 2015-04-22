library(Rcpp)
sourceCpp('problem73.cpp')

CountRange <- function(n) {
    a <- 1
    b <- 3
    neighbor <- FindNeighbor(a,b,n)
    p <- neighbor[1]
    q <- neighbor[2]
    countadj(a,b,p,q,n)
}

FindNeighbor <- function(a,b,n) {
    for (q in n:1) {
        for (p in 1:n) {
            if (b*p-1==a*q) return (c(p,q))
        }
    }
}

# test
#CountRange(8)==3

#a <- CountRange(12000)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# CountRange(12000) 588.7547 593.3232 610.8588 598.0711 638.0122 651.1399    10
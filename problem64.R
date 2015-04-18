# Exactly four continued fractions, for N ≤ 13, have an odd period.
# 
# How many continued fractions for N ≤ 10000 have an odd period?

OddPeriodSquareRoots <- function(n) {
    sum(sapply(2:n, OddPeriod))
}

OddPeriod <- function(n) {
    if (IsSquare(n)) return(FALSE)
    s <- sqrt(n)
    x <- s
    f <- floor(s)
    P <- c(0)
    Q <- c(1)
    k <- 1
    repeat {
        P <- c(P, f*Q[k]-P[k])
        Q <- c(Q, (n-P[k+1]^2)/Q[k])
        k <- k+1
        for (i in 1:(k-1)) {
            if (P[i]==P[k] && Q[i]==Q[k]) return((k-i)%%2!=0)
        }
        x <- (P[k]+s)/Q[k]
        f <- floor(x)
    }
}

IsSquare <- function(n) {
    sqrt(n)%%1==0
}

# test
#OddPeriodSquareRoots(13)==4

#a <- OddPeriodSquareRoots(10000)

# Unit: seconds
# expr         min          lq        mean      median          uq         max neval
# OddPeriodSquareRoots(10000) 7.505542631 7.505542631 7.505542631 7.505542631 7.505542631 7.505542631     1
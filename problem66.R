# Consider quadratic Diophantine equations of the form:
#
#     x^2 – D*y^2 = 1
#
# For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
# It can be assumed that there are no solutions in positive integers when D is square.
#
# By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
#
#         3^2 – 2×2^2 = 1
#         2^2 – 3×1^2 = 1
#         9^2 – 5×4^2 = 1
#         5^2 – 6×2^2 = 1
#         8^2 – 7×3^2 = 1
#
# Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
#
# Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.

library(gmp)
options(scipen=999)

DiophantineE <- function(n) {
    which.max(sapply(1:n, SolX))
}

SolX <- function(n) {
    if (IsSquare(n)) return(0)
    a <- ContFract(n)
    sub.a <- a[-1]
    P <- as.bigz(c(a[1],a[1]*a[2]+1))
    Q <- as.bigz(c(1, a[2]))
    i <- 3
    while (as.bigz(tail(P,1))^2-n*as.bigz(tail(Q,1))^2!=1) {
        if (i==length(a)+1) a <- c(a, sub.a)
        P <- c(P, a[i]*P[i-1]+P[i-2])
        Q <- c(Q, a[i]*Q[i-1]+Q[i-2])
        i <- i+1
    }
    as.character(tail(P,1))
}

ContFract <- function(n) {
    if (IsSquare(n)) return(0)
    s <- sqrt(n)
    f <- floor(s)
    P <- c(0)
    Q <- c(1)
    k <- 1
    alpha <- c(f)
    repeat {
        P <- c(P, f*Q[k]-P[k])
        Q <- c(Q, (n-P[k+1]^2)/Q[k])
        k <- k+1
        for (i in 1:(k-1)) {
            if (P[i]==P[k] && Q[i]==Q[k]) return(alpha)
        }
        x <- (P[k]+s)/Q[k]
        f <- floor(x)
        alpha <- c(alpha, f)
    }
}

IsSquare <- function(n) {
    sqrt(n)%%1==0
}

# test
#DiophantineE(7)==5
a <- DiophantineE(1000)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# DiophantineE(1000) 4.271005 4.347557 4.378391 4.364683 4.432114 4.463417    10


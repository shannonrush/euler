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
        if (length(alpha)>100) return(alpha)
    }
}

Convergent <- function(n, index) {
    if (IsSquare(n)) return(0)
    a <- ContFract(n)
    sub.a <- a[-1]
    P <- as.bigz(c(a[1],a[1]*a[2]+1))
    Q <- as.bigz(c(1, a[2]))
    i <- 3
    if (index<3) return(c(P[index],Q[index]))
    repeat {
        if (i==length(a)+1) a <- c(a, sub.a)
        P <- c(P, a[i]*P[i-1]+P[i-2])
        Q <- c(Q, a[i]*Q[i-1]+Q[i-2])
        if (i==index) return(c(as.character(tail(P,1)),as.character(tail(Q,1))))
        i <- i+1
    }
}

# this was the solution to #66, Ps are num, Qs are den
SolvePell <- function(n) {
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
    c(as.character(tail(P,1)),as.character(tail(Q,1)))
}

IsSquare <- function(n) {
    sqrt(n)%%1==0
}

# tests:
# ContFract for e
# ContFract for arbitrary decimals
# ContFract for pi

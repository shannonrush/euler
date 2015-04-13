# The number, 197, is called a circular prime because all rotations of the digits: 
# 197, 971, and 719, are themselves prime.
# 
# There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
# 
# How many circular primes are there below one million?

CircPrimes <- function(n) {
    circs <- c()
    for (i in 2:(n-1)) {
        if (i<8 || (i%%2!=0 && i%%3!=0 && i%%5!=0 && i%%7!=0)) {
            c.v <- IsCircular(i, circs)
            if (c.v[[1]]) circs <- c(circs, c.v[[2]])
        }
    }
    length(circs)
}

IsCircular <- function(n, circs) {
    if (n %in% circs) return(list(TRUE,c()))
    if (nchar(n)==1) return(list(IsPrime(n),n))
    digits <- as.integer(unlist(strsplit(toString(n),"")))
    r <- c()
    next.r <- as.integer(paste(digits,collapse=""))
    while (!next.r %in% r) {
        if (!IsPrime(next.r)) return(list(FALSE))
        r <- c(r, next.r)
        digits <- c(digits[2:length(digits)], digits[1])
        next.r <- as.integer(paste(digits,collapse=""))
    }
    list(TRUE,r)
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m < max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}

# test 
#CircPrimes(100)==13
#IsCircular(197,c())[[1]]==TRUE

#CircPrimes(1000000)

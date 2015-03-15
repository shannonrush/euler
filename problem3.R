# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143?

PrimeFactors <- function(x) {
    factors <- c()
    for (i in 2:sqrt(x)) {
        if (IsPrime(i) & x%%i==0) factors <- c(factors, i)
        print(i)
    }
    factors
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

f <- PrimeFactors(13195)
# [1]  5  7 13 29

answer <- tail(PrimeFactors(600851475143), 1)
# [1] 6857




# Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
# 
# 37 36 35 34 33 32 31
# 38 17 16 15 14 13 30
# 39 18  5  4  3 12 29
# 40 19  6  1  2 11 28
# 41 20  7  8  9 10 27
# 42 21 22 23 24 25 26
# 43 44 45 46 47 48 49
# 
# It is interesting to note that the odd squares lie along the bottom right diagonal, 
# but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
# 
# If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. 
# If this process is continued, what is the side length of the square spiral for which the ratio of primes along 
# both diagonals first falls below 10%?

SpiralPrimes <- function() {
    numbers <- 1
    primes <- 0
    p <- 1
    by <- 2
    start <- 1
    ratio <- 1
    i <- 1
    while (ratio >= 0.1) { # 10%
        start <- start + (2*i)
        end <- start+6*i
        diagonals <- seq(start,end,by=by)
        numbers <- numbers + length(diagonals)
        primes <- primes + sum(sapply(diagonals[1:3], IsPrime))
        ratio <- primes/numbers
        by<-by+2
        start <- end
        i <- i+1
    }
    by-1
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

# a<-SpiralPrimes()

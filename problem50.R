# The prime 41, can be written as the sum of six consecutive primes:
# 
#     41 = 2 + 3 + 5 + 7 + 11 + 13
# 
# This is the longest sum of consecutive primes that adds to a prime below one-hundred.
# 
# The longest sum of consecutive primes below one-thousand that adds to a prime, 
# contains 21 terms, and is equal to 953.
# 
# Which prime, below one-million, can be written as the sum of the most consecutive primes?

ConsecPrimes <- function(n) {
    max.length <- 0
    max.sum <- 0
    p <- 2
    while (p<n) {
        l <- 1
        prime <- p
        sum <- prime
        while (sum<n) {
            if (IsPrime(sum) && l>max.length) {
                max.length <- l
                max.sum <- sum
            } 
            prime <- NextPrime(prime)
            sum <- sum+prime
            l <- l+1
        }
        p <- NextPrime(p)
    }
    max.sum
}

NextPrime <- function(n) {
    n <- n+1
    while (!IsPrime(n)) n<-n+1
    n
}

IsPrime <- function(n) {
    m <- 2
    max <- n
    while (m<max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}

# test
#ConsecPrimes(100)==41
#ConsecPrimes(1000)==953

#a<-ConsecPrimes(1000000)

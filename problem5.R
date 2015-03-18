# 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
# 
# What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

SmallestMult <- function(n) {
    primes <- PrimesTo(n)
    x <- n
    while(sum(sapply(2:n, function(i) x%%i))>0) {
        x <- x+n
        while(sum(sapply(primes, function(j) x%%j))>0) {
            x <- x+n
        }
    }
    x
}

PrimesTo <- function(n) {
    primes <- c(2)
    for (i in 3:n) if (IsPrime(i)) primes <- c(primes, i)
    primes
}

IsPrime <- function(n) {
    all(sapply(2:(n-1), function(i) n%%i!=0))
}

SmallestMult(10)
# [1] TRUE

SmallestMult(20)
# [1] 232792560

# O(n^2)
 
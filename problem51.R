# By replacing the 1st digit of the 2-digit number *3, 
# it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
# 
# By replacing the 3rd and 4th digits of 56**3 with the same digit, 
# this 5-digit number is the first example having seven primes among the ten generated numbers, 
# yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. 
# Consequently 56003, being the first member of this family, is the smallest prime with this property.
# 
# Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) 
# with the same digit, is part of an eight prime value family.
options(scipen=999)

PrimeReplace <- function() {
    for (p in PossiblePrimes()) if (HasEightPrimes(p)) return(p)
}

HasEightPrimes <- function(p) {
    digits <- unlist(strsplit(toString(p),""))
    repeating <- RepeatingDigit(digits)
    prime.family <- c()
    repeat.i <- which(digits==repeating)
    for (i in as.character(0:9)) {
        digits[repeat.i] <- i
        if (digits[1]!="0"){
            p <- as.numeric(paste(digits, collapse=""))
            if (IsPrime(p)) prime.family<-c(prime.family, p)
        }  
    }
    length(prime.family)==8
}

RepeatingDigit <- function(digits) {
    for (d in c("0","1","2")) {
        if (sum(digits==d)==3) return(d)
    } 
}

PossiblePrimes <- function() {
    p <- c()
    for (i in 100000:999999) {
        digits <- unlist(strsplit(toString(i),""))
        if (CorrectRepeating(digits)) {
            if (IsPrime(i)) p<-c(p,i)
        }
    }
    p
}

CorrectRepeating <- function(digits) {
    for (d in c("0","1","2")) {
        if (sum(digits==d)==3 && digits[6]!=d) return(TRUE)
    }
    FALSE
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

# a <- PrimeReplace()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PrimeReplace() 22.44465 22.44465 22.44465 22.44465 22.44465 22.44465     1
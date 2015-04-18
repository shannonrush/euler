# The 5-digit number, 16807=7^5, is also a fifth power. 
# Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
# 
# How many n-digit positive integers exist which are also an nth power?

PowerfulDigits <- function() {
    power.digits <- c()
    length.diff <- 1
    n <- 1
    while (length.diff>0) {
        start <- length(power.digits)
        i <- 1
        while (nchar(i^n)<=n) {
            if(nchar(i^n)==n) power.digits <- c(power.digits, i^n)
            i <- i+1
        }
        n <- n+1
        length.diff <- length(power.digits)-start
    } 
    length(power.digits)
}

#a <- PowerfulDigits()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# PowerfulDigits() 1.710878 1.784368 1.942519 1.844389 1.925416 3.745215   100

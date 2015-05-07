# The Fibonacci sequence is defined by the recurrence relation:
#     
# Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
# 
# It turns out that F541, which contains 113 digits, is the first Fibonacci number for which the last nine digits are 
# 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). 
# And F2749, which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.
# 
# Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, 
# find k.

library(gmp)

PanFib <- function() {
    f1 <- as.bigz(1)
    f2 <- as.bigz(1)
    i <- 2
    repeat {
        last <- as.character(f2)
        if (nchar(last)>=9) {
            digits <- as.integer(unlist(strsplit(last,"")))
            if (IsPandigital(head(digits,9)) && IsPandigital(tail(digits,9))) return(i)
        }
        f <- f2+f1
        f1 <- f2
        f2 <- f
        i <- i+1
    }
}

IsPandigital <- function(digits) {
    all(sort(head(digits,9))==1:9)
}

#a <- PanFib()

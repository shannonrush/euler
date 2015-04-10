# The Fibonacci sequence is defined by the recurrence relation:
#     
#     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
# Hence the first 12 terms will be:
#     
#     F1 = 1
# F2 = 1
# F3 = 2
# F4 = 3
# F5 = 5
# F6 = 8
# F7 = 13
# F8 = 21
# F9 = 34
# F10 = 55
# F11 = 89
# F12 = 144
# The 12th term, F12, is the first term to contain three digits.
# 
# What is the first term in the Fibonacci sequence to contain 1000 digits?
library(gmp)

FibDigit <- function(target) {
    n <- as.bigz(1)
    l <- 1
    s <- 2
    while (nchar(as.character(n,b=10))<target) {
        tmp <- n+l
        l <- n
        n <- tmp
        s <- s+1
    }
    s
}

# test 
#FibDigit(3)==12

#FibDigit(1000)
#[1] 4782

# expr        min          lq        mean     median         uq        max neval
# FibDigit(3)    227.573    237.0345    293.7498    247.978    256.795   4576.365   100
# FibDigit(1000) 160135.985 165817.8940 169146.9532 167324.190 170083.421 229699.093   100
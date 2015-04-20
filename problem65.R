# The first ten terms in the sequence of convergents for e are:
#
# 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
# The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
#
# Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.
options(scipen=999)
library(gmp)

ConvE <- function(n) {
    sum(as.integer(unlist(strsplit(toString(Num(n)),""))))
}

Num <- function(x) {
    d <- 1
    n <- 2
    for (i in 2:x) {
        t <- as.bigz(d)
        c <- ifelse(i%%3==0, 2*(i/3), 1)
        d <- n
        n <- c*d+t
    }
    n
}


# test
#ConvE(10)==17

#a <- ConvE(100)

# Unit: milliseconds
# expr     min        lq       mean   median       uq      max neval
# ConvE(100) 3.33868 3.4704785 3.59767361 3.528419 3.621416 5.391353   100

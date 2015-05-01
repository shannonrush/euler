# It is easily proved that no equilateral triangle exists with integral length sides and integral area. 
# However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
# 
# We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third 
# differs by no more than one unit.
# 
# Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and 
# whose perimeters do not exceed one billion (1,000,000,000).
options(scipen=999)

AlmostEquilateral <- function() {
    L <- 1000000000
    total.p <- 0
    C <- GenerateC(L)
    sign <- 1
    for (c in C) {
        p <- 3*c+sign
        if (p<L) total.p <- total.p + p
        sign <- -sign
    }
    total.p
}

GenerateC <- function(L) {
    a <- 3
    b <- 4
    c <- 5
    C <- c(c)
    sign <- -1
    while (c<(L/3)) {
        a0 <- a
        b0 <- b
        a <- b0+c+sign
        b <- c+(2*b0)+a0+sign
        sign <- -sign
        c <- 2*a+sign
        C <- c(C,c)
    }
    C
}

# a <- AlmostEquilateral()

# Unit: microseconds
# expr    min      lq     mean median      uq     max neval
# AlmostEquilateral() 50.594 58.9615 60.37677 59.774 60.7085 137.673   100

# notes
# First in series: (5, 5, 6), (17, 17, 16), (65, 65, 66), (241, 241, 240) and (901, 901, 902).

# This is an infinite series; two sides are equal in length to the hypotenuse of almost 30-60 triangles 
# and the third side ALTERNATES between that length +/- 1.

# Hypotenuse sequence: http://www.uwgb.edu/dutchs/RECMATH/rmpowers.htm#almost30

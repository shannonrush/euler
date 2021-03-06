# Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:
#     
# Triangle     	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
# Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
# Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
# 
# It can be verified that T285 = P165 = H143 = 40755.
# 
# Find the next triangle number that is also pentagonal and hexagonal.

TriPentHex <- function() {
    i <- 286
    n <- (i*(i+1))/2
    while (!(IsPent(n) && IsHex(n))) {
        i <- i+1
        n <- (i*(i+1))/2
    }
    n
}

IsPent <- function(n) {
    ((1+sqrt(24*n+1))/6)%%1==0
}

IsHex <- function(n) {
    ((1+sqrt(8*n+1))/4)%%1==0
}

# TriPentHex()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# TriPentHex() 137.7758 149.3899 153.4354 152.2861 154.5801 201.6696   100
# Consider the fraction, n/d, where n and d are positive integers. 
# If n<d and HCF(n,d)=1 (note: hcf==gcd), it is called a reduced proper fraction.
# 
# If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
#     
# 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
# 
# It can be seen that 2/5 is the fraction immediately to the left of 3/7.
# 
# By listing the set of reduced proper fractions for d ≤ 1,000,000 
# in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.
library(MASS)

# find all reduced fractions with d <= 10^6 and <= 3/7
OrderedFractions <- function(n) {
    a <- 2
    b <- 5
    p <- 3
    q <- 7
    M.n <- a+p
    M.d <- b+q
    num <- c(M.n)
    den <- c(M.d)
    while (M.d<=n) {
        print(paste(M.n,"/",M.d))
        a <- M.n
        b <- M.d
        M.n <- a+p
        M.d <- b+q
        num <- c(num, M.n)
        den <- c(den, M.d)
    }
    num[length(num)-1]
}

# test
#OrderedFractions(8)==2

ans <- OrderedFractions(10^6)

# # We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; 
# for example, the 5-digit number, 15234, is 1 through 5 pandigital.
# # 
# # The product 7254 is unusual, as the identity, 39 × 186 = 7254, 
# containing multiplicand, multiplier, and product is 1 through 9 pandigital.
# # 
# # Find the sum of all products whose multiplicand/multiplier/product 
# identity can be written as a 1 through 9 pandigital.
# # 
# # HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

PandigitalProds <- function() {
    pans <- c()
    for (i in 1234:9876) {
        if (IsPandigital(i)) pans <- c(pans, i)
    }
    sum(unique(pans))
}

IsPandigital <- function(n) {
    for (i in 2:ceiling(n/2)) {
        if (n%%i==0) {
            digits <- sort(as.integer(unlist(strsplit(unlist(strsplit(paste(c(i, n/i, n),collapse="")," ")),""))))
            if (identical(digits,1:9)) return(TRUE)
        }
    }
    FALSE
}

# test
# IsPandigital(7254)==TRUE

# PandigitalProds()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PandigitalProds() 17.52787 17.58643 17.83693 17.62375 18.06366 18.75458    10
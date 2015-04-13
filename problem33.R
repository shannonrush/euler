library(MASS)

DigitCancelFracts <- function() {
    num <- 1
    den <- 1
    for (i in 11:99) {
        for (j in 11:99) {
            if (CuriousFraction(i,j)) {
                num <- num * i
                den <- den * j
            }
        }
    }
    as.integer(unlist(strsplit(attributes(fractions(num/den))$fracs,"/"))[2])
}

CuriousFraction <- function(i,j) {
    if (i<j && i%%10!=0) {
        a <- unlist(strsplit(toString(i),""))
        b <- unlist(strsplit(toString(j),""))
        if (any(a %in% b)) {
            match <- a[a%in%b][1]
            a <- as.integer(a[-which(a==match)[1]])
            b <- as.integer(b[-which(b==match)[1]])
            if (a!=0 && b!=0) {
                if (a/b==i/j) return(TRUE)
            }
        }
    }
    FALSE
}

# test
#CuriousFraction(49, 98)==TRUE

#DigitCancelFracts()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# DigitCancelFracts() 131.6445 133.1243 136.5872 134.8651 136.9782 175.7332   100
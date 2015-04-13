# The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
# 
# Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
# 
# (Please note that the palindromic number, in either base, may not include leading zeros.)

DoubleBasePal <- function() {
    sum <- 0
    for (i in 1:999999) if (IsDoublePal(i)) sum <- sum + i
    sum 
}

IsDoublePal <- function(n) {
    s <- unlist(strsplit(toString(n),""))
    if (!IsPal(s)) return(FALSE)
    b2 <- unlist(strsplit(ToBase(n, 2),""))
    if (b2[1]=="0" || !IsPal(b2)) return(FALSE)
    TRUE
}

IsPal <- function(s) {
    identical(s,rev(s))
}

ToBase <- function(n, base) {
    symbols <- c(as.character(0:9), LETTERS)
    max.len <- trunc(log(max(n, 1)) / log(base)) + 1
    power <- rep(1, length(n)) * base^((max.len-1):0)
    n <- n * rep(1, max.len)
    digits <- floor((n %% (base*power)) / power)
    paste(symbols[digits+1], collapse="")
}

# test 

#IsDoublePal(585)==TRUE

#DoubleBasePal()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# DoubleBasePal() 25.26462 26.07535 26.29613 26.30042 26.84423 27.08768    10
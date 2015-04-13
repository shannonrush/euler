# The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, 
# but it also has a rather interesting sub-string divisibility property.
# 
# Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
#     
# d2d3d4=406 is divisible by 2
# d3d4d5=063 is divisible by 3
# d4d5d6=635 is divisible by 5
# d5d6d7=357 is divisible by 7
# d6d7d8=572 is divisible by 11
# d7d8d9=728 is divisible by 13
# d8d9d10=289 is divisible by 17

# Find the sum of all 0 to 9 pandigital numbers with this property.

SubstringDiv <- function() {
    primes <- c(3,5,7,11,13,17)
    results <- DivBy(2)
    for (p in primes) {
        for (result in results) {
            unused <- setdiff(0:9, unlist(strsplit(result,"")))
            for (u in unused) {
                a <- paste0(substr(result,nchar(result)-1,nchar(result)),u)
                if (as.integer(a)%%p==0) results <- c(results, paste0(result,u))
            }
        }
    }
    sum(sapply(results[nchar(results)==9], AddMissing))
}

AddMissing <- function(result) {
    missing <- setdiff(0:9, as.integer(unlist(strsplit(result,""))))
    if (missing==0) return(0)
    as.numeric(paste0(missing,result))
}

DivBy <- function(n) {
    d <- c()
    for (i in 12:987) {
        if (i%%n==0) {
            r <- sprintf("%03s",i)
            if (!any(duplicated(unlist(strsplit(r,""))))) d<-c(d,r)
        }
    }
    d
}

# Unit: seconds
# expr      min      lq     mean   median       uq     max neval
# SubstringDiv() 3.844737 3.86282 3.931229 3.908966 3.974604 4.15545    10

# a <- SubstringDiv()

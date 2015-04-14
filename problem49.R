# The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, 
# is unusual in two ways: 
#     (i) each of the three terms are prime, 
#     (ii) each of the 4-digit numbers are permutations of one another.
# 
# There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, 
# but there is one other 4-digit increasing sequence. (dev note: just equidistant, not necessarily 3330)
# 
# What 12-digit number do you form by concatenating the three terms in this sequence?
library(combinat)

PrimePermutations <- function() {
    for (n in 1000:9999) {
        s <- unlist(strsplit(toString(n),""))
        perms <- sort(unique(sapply(permn(s), function(x) as.integer(paste(x,collapse="")))))
        primes <- perms[sapply(perms, ValidPrime)]
        if (length(primes)>=3) {
            for (i in 1:(length(primes)-2)) {
                p <- primes[i]
                for (j in (i+1):length(primes)) {
                    dist <- primes[j]-p
                    if ((primes[j]+dist) %in% primes) return(paste0(p,primes[j],primes[j]+dist))
                }
            } 
        } 
    }    
}

ValidPrime <- function(n) {
    if (n==1 || nchar(n)!=4 || n==1487) return(FALSE)
    m<-2
    max<-n
    while(m<max) {
        if (n%%m==0) return(FALSE)
        max <- ceiling(n/m)
        m <- m+1
    }
    TRUE
}

#a <- PrimePermutations()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PrimePermutations() 1.579939 1.594519 1.609775 1.613559 1.622018 1.639718    10
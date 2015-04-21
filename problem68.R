library(e1071)
MagicRing <- function(n) {
    perms <- permutations(10)
    a <- perms[which(apply(perms,1,EqualSums)),]
    b <- a[which(apply(a,1,CorrectStart)),]
    possibles <- apply(b,1,Concat)
    max(possibles)
}

Concat <- function(x) {
    paste0(x[1],x[6],x[7],x[2],x[7],x[8],x[3],x[8],x[9],x[4],x[9],x[10],x[5],x[10],x[6],collapse="")
}

EqualSums <- function(x) {
    sum <- sum(x[c(1,6,7)])
    if (sum(x[c(2,7,8)])!=sum) return(FALSE)
    if (sum(x[c(3,8,9)])!=sum) return(FALSE)
    if (sum(x[c(4,9,10)])!=sum) return(FALSE)
    if (sum(x[c(5,10,6)])!=sum) return(FALSE)
    TRUE
}

CorrectStart <- function(x) {
    min(x[c(1:5)])==x[1]
}
    
#a <- MagicRing()
# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# MagicRing() 27.67656 27.67656 27.67656 27.67656 27.67656 27.67656     1
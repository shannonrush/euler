# If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
# then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
# 
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, 
# how many letters would be used?
# 
# NOTE: Do not count spaces or hyphens. 
# For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) 
# contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

NumberLetterCounts <- function(numbers) {
    strings <- sapply(numbers, NumberString)
    sum(sapply(strings, function(x) length(unlist(strsplit(x,"")))))
}

NumberString <- function(n) {
    n <- toString(n)
    if (n=="1000") return("onethousand")
    s <- ""
    if (n=="100") return("onehundred")
    if (as.integer(n)>100) {
        t <- unlist(strsplit(n,""))
        s <- paste0(s, NumWord(as.integer(head(t,1))), "hundred")
        if (t[2]=="0"&&t[3]=="0") {
            return(s)
        } else {
            s <- paste0(s,"and")
            n <- paste(t[-1], collapse="")   
        }
    }
    if (as.integer(n)<20) return(paste0(s, NumWord(as.integer(n))))
    t <- unlist(strsplit(n,""))
    s <- paste0(s, NumTens(as.integer(head(t,1))))
    t <- t[-1]
    if (t[1]=="0") return(s)
    paste0(s, NumWord(as.integer(t[1])))
}

NumWord <- function(n) {
    if (n==1) {
        return("one")
    } else if (n==2) {
        return("two")
    } else if (n==3) {
        return("three")
    } else if (n==4) {
        return("four")
    } else if (n==5) {
        return("five")
    } else if (n==6) {
        return("six")
    } else if (n==7) {
        return("seven")
    } else if (n==8) {
        return("eight")
    } else if (n==9) {
        return("nine")
    } else if (n==10) {
        return("ten")
    } else if (n==11) {
        return("eleven")
    } else if (n==12) {
        return("twelve")
    } else if (n==13) {
        return("thirteen")
    } else if (n==14) {
        return("fourteen")
    } else if (n==15) {
        return("fifteen")
    } else if (n==16) {
        return("sixteen")
    } else if (n==17) {
        return("seventeen")
    } else if (n==18) {
        return("eighteen")
    } else if (n==19) {
        return("nineteen")
    } else {
        return("")
    }
}

NumTens <- function(n) {
    if (n==2) {
        return("twenty")
    } else if (n==3) {
        return("thirty")
    } else if (n==4) {
        return("forty")
    } else if (n==5) {
        return("fifty")
    } else if (n==6) {
        return("sixty")
    } else if (n==7) {
        return("seventy")
    } else if (n==8) {
        return("eighty")
    } else if (n==9) {
        return("ninety")
    }
}

# tests
#NumberLetterCounts(c(342))==23
#NumberLetterCounts(115)==20

# NumberLetterCounts(1:1000)
# 21124

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# NumberLetterCounts(1:1000) 82.50824 86.85741 89.73152 89.31784 91.53788 104.3014   100


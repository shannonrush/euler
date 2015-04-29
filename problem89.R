# For a number written in Roman numerals to be considered valid there are basic rules which must be followed. Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.
# 
# For example, it would appear that there are at least six ways of writing the number sixteen:
#     
# IIIIIIIIIIIIIIII
# VIIIIIIIIIII
# VVIIIIII
# XIIIIII
# VVVI
# XVI
# 
# However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most efficient, as it uses the least number of numerals.
# 
# The 11K text file, roman.txt, contains one thousand numbers written in valid, but not necessarily minimal, Roman numerals; see About... Roman Numerals for the definitive rules for this problem.
# 
# Find the number of characters saved by writing each of these in their minimal form.
# 
# Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.

RomanNumerals <- function() {
    dict <- c("I"=1,"V"=5,"X"=10,"L"=50,"C"=100,"D"=500,"M"=1000)
    numerals <- read.table("resources/p089_roman.txt", stringsAsFactors=F)$V1
    sum(sapply(numerals, CharsSaved, dict))
}

CharsSaved <- function(numeral, dict) {
    n <- RNToInt(numeral,dict)
    valid <- IntToRN(n) 
    max(0,nchar(numeral)-nchar(valid))
}

IntToRN <- function(n) {
    rn <- c()
    if (n>=1000) {
        rn <- c(rn, rep("M",floor(n/1000)))
        n <- n%%1000
    } 
    while (n>=100) {
        if (n>=900) {
            rn <- c(rn,"C","M")
            n <- n-900
        } else if (n>=500) {
            rn <- c(rn, "D")
            n <- n-500
        } else if (n>=400) {
            rn <- c(rn, "C","D")
            n <- n-400
        } else {
            rn <- c(rn, rep("C",floor(n/100)))
            n <- n%%100
        }
    }
    while (n>=10) {
        if (n>=90) {
            rn <- c(rn, "X","C")
            n<-n-90
        } else if (n>=50) {
            rn <- c(rn, "L")
            n<-n-50
        } else if (n>=40) {
            rn <- c(rn, "X","L")
            n <- n-40
        } else {
            rn <- c(rn, rep("X",floor(n/10)))
            n <- n%%10
        }
    }
    if (n==9) {
        rn <- c(rn, "I","X")
    } else if (n>=5) {
        rn <- c(rn, "V", rep("I",n-5))
    } else if (n==4) {
        rn <- c(rn, "I","V")
    } else {
        rn <- c(rn, rep("I",n))
    }
    paste(rn, collapse="")
}

RNToInt <- function(numeral, dict) {
    r <- rev(unlist(strsplit(numeral,"")))
    sum <- dict[r[1]]
    if (length(r)==1) return(sum)
    for (i in 2:length(r)) {
        if (dict[r[i]]<dict[r[i-1]]) {
            sum <- sum-dict[r[i]]
        } else {
            sum <- sum+dict[r[i]]
        }
    }
    unname(sum)
}

# test
#RNToInt("MMMMDCLXXII",dict)==4672
#RNToInt("CMLXIX",dict)==969
#IntToRN(590)=="DXC"

#a <- RomanNumerals()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# RomanNumerals() 83.51662 87.94063 90.98234 90.09579 92.59676 158.0479   100

# Take the number 192 and multiply it by each of 1, 2, and 3:
#     
# 192 × 1 = 192
# 192 × 2 = 384
# 192 × 3 = 576
# 
# By concatenating each product we get the 1 to 9 pandigital, 192384576. 
# We will call 192384576 the concatenated product of 192 and (1,2,3)
# 
# The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, 
# which is the concatenated product of 9 and (1,2,3,4,5).
# 
# What is the largest 1 to 9 pandigital 9-digit number that can be formed 
# as the concatenated product of an integer with (1,2, ... , n) where n > 1?
library(combinat)

PanMult <- function() {
    pans <- sort(sapply(permn(1:9), function(x) as.integer(paste(x,collapse=""))), decreasing=T)
    for (pan in pans) if (Concatable(pan)) return(pan)
}

Concatable <- function(pan) {
    digits <- as.integer(unlist(strsplit(toString(pan),"")))
    for (i in 1:8) { 
        n <- as.integer(paste(head(digits,i),collapse=""))
        j <- 1
        cat <- c(as.integer(unlist(strsplit(toString(n*j),""))))
        while (length(cat)<=9 && all(cat==digits[1:length(cat)])) {
            if (length(cat)==9) return(TRUE)
            j <- j+1
            cat <- c(cat, as.integer(unlist(strsplit(toString(n*j),""))))   
        }
    }
    FALSE
}

# test
#Concatable(192384576)==TRUE
#Concatable(918273645)==TRUE

#max.pan <- PanMult()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# PanMult() 25.98512 25.98512 25.98512 25.98512 25.98512 25.98512     1

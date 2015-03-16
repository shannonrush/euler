# A palindromic number reads the same both ways. 
# The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
# 
# Find the largest palindrome made from the product of two 3-digit numbers.

LargestPalindrome <- function(digits) {
    num <- as.integer(paste(rep(9,digits),collapse=""))
    least <- 10^(digits-1)
    pals <- c()
    for (i in num:least) {
        for (j in num:least) {
            if (IsPalindrome(i*j)) pals <- c(pals, i*j)
        }
    }
    max(pals)
}

IsPalindrome <- function(int) {
    s <- unlist(strsplit(toString(int),""))
    identical(s, rev(s))
}

LargestPalindrome(2)==9009
# [1] TRUE

LargestPalindrome(3)
# [1] 580085

# O(n * n) = O(n^2)
# A permutation is an ordered arrangement of objects. 
# For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. 
# If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. 
# The lexicographic permutations of 0, 1 and 2 are:
#     
#     012   021   102   120   201   210
# 
# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

LexPerms <- function() {
    library(gtools)
    p<-permutations(10,10,0:9)
    p[1000000,]
}

#answer <- LexPerms()
#[1] 2 7 8 3 9 1 5 4 6 0

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# LexPerms() 61.17119 61.17119 61.17119 61.17119 61.17119 61.17119     1
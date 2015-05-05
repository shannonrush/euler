# Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for any two non-empty 
# disjoint subsets, B and C, the following properties are true:
#     
# S(B) ≠ S(C); that is, sums of subsets cannot be equal.
# If B contains more elements than C then S(B) > S(C).
# 
# If S(A) is minimised for a given n, we shall call it an optimum special sum set. 
# The first five optimum special sum sets are given below.
# 
# n = 1: {1}
# n = 2: {1, 2}
# n = 3: {2, 3, 4}
# n = 4: {3, 5, 6, 7}
# n = 5: {6, 9, 11, 12, 13}
# 
# It seems that for a given optimum set, A = {a1, a2, ... , an}, the next optimum set is of the form 
# B = {b, a1+b, a2+b, ... ,an+b}, where b is the "middle" element on the previous row.
# 
# By applying this "rule" we would expect the optimum set for n = 6 to be 
# A = {11, 17, 20, 22, 23, 24}, with S(A) = 117. 
# However, this is not the optimum set, as we have merely applied an algorithm to provide a near optimum set. 
# The optimum set for n = 6 is A = {11, 18, 19, 20, 22, 25}, with S(A) = 115 and corresponding set string: 111819202225.
# 
# Given that A is an optimum special sum set for n = 7, find its set string.

OptimumSpecial <- function() {
    # use the rule set in problem to get target
    t <- c(20,31,38,39,40,42,45)
    sets <- t(combn(19:46,7))
    r <- 1
    for (i in 1:7) {
        sets <- sets[sets[,i] %in% (t[i]-r):(t[i]+r),]
    }
    optimum.min <- Inf
    optimum.special <- ""
    for (i in 1:nrow(sets)) {
        A <- sets[i,]
        if (sum(A)<optimum.min) {
            if (Optimum(A)) {
                optimum.min <- sum(A)
                optimum.special <- paste(A,collapse="")
            }   
        }
    }
    return(optimum.special)
}

Optimum <- function(A) {
    if (tail(A,1)>sum(A[1:2])) return(FALSE)
    if (any(Reduce(sum,head(A,length(A)-1),accumulate=T)==tail(A,1))) return(FALSE)
    if (any(A[2:6]+A[1]==A[7])) return(FALSE)
    for (k in 1:(length(A)-1)) {
        all.B <- t(combn(A,k))
        for (i in 1:nrow(all.B)) {
            B <- all.B[i,]
            remaining <- A[!A %in% B]
            # C can be any number of elements
            for (l in 1:length(remaining)) {
                all.C <- t(combn(remaining,l))
                for (j in 1:nrow(all.C)) {
                    C <- all.C[j,]
                    if (!ConditionsMet(B,C)) return(FALSE)       
                }
            }
        }
    }
    TRUE
}

ConditionsMet <- function(B,C) {
    if (sum(B)==sum(C)) return(FALSE)
    if (length(B) > length(C)) if (sum(C)>sum(B)) return(FALSE)
    TRUE
}

#a <- OptimumSpecial()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# OptimumSpecial() 15.69182 15.69182 15.69182 15.69182 15.69182 15.69182     1

# notes
# Special if: 
# For all subsets B and C:
# Sum B can't equal sum C
# If length B > length C, sum B > sum C

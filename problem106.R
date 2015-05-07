# Let S(A) represent the sum of elements in set A of size n. 
# We shall call it a special sum set if for any two non-empty disjoint subsets, B and C, the following properties are true:
#     
# S(B) ≠ S(C); that is, sums of subsets cannot be equal.
# If B contains more elements than C then S(B) > S(C).
# 
# For this problem we shall assume that a given set contains n strictly increasing elements and it already satisfies 
# the second rule.
# 
# Surprisingly, out of the 25 possible subset pairs that can be obtained from a set for which n = 4, 
# only 1 of these pairs need to be tested for equality (first rule). 
# 
# Similarly, when n = 7, only 70 out of the 966 subset pairs need to be tested.
# 
# For n = 12, how many of the 261625 subset pairs that can be obtained need to be tested for equality?

EqualityTests <- function(n) {
    EqualTests(1:n)
}

EqualTests <- function(A) {
    sum <- 0
    for (k in 1:(length(A)-1)) {
        all.B <- t(combn(A,k))
        for (i in 1:nrow(all.B)) {
            B <- all.B[i,]
            remaining <- A[!A %in% B]
            for (l in 1:length(remaining)) {
                all.C <- t(combn(remaining,l))
                for (j in 1:nrow(all.C)) {
                    C <- all.C[j,]
                    sum <- sum+NeedsEqual(A,B,C)       
                }
            }
        }
    }
    sum/2
}

NeedsEqual <- function(A,B,C) {
    if (length(B)!=length(C)) return(0)
    if (length(B)==1) return(0)
    if (all(B<C) || all(C<B)) return(0)
    1
}

#test
# EqualityTests(4)==1
# EqualityTests(7)==70
#a<-EqualityTests(12)

# Unit: seconds
# expr     min      lq    mean  median      uq     max neval
# EqualityTests(12) 6.03741 6.03741 6.03741 6.03741 6.03741 6.03741     1
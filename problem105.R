# Let S(A) represent the sum of elements in set A of size n. We shall call it a special sum set if for 
# any two non-empty disjoint subsets, B and C, the following properties are true:
#     
# S(B) ≠ S(C); that is, sums of subsets cannot be equal.
# If B contains more elements than C then S(B) > S(C).
# 
# For example, {81, 88, 75, 42, 87, 84, 86, 65} is not a special sum set because 65 + 87 + 88 = 75 + 81 + 84, 
# whereas {157, 150, 164, 119, 79, 159, 161, 139, 158} satisfies both rules for all possible subset pair combinations 
# and S(A) = 1286.
# 
# Using sets.txt, a 4K text file with one-hundred sets containing seven to twelve elements 
# (the two examples given above are the first two sets in the file), 
# identify all the special sum sets, A1, A2, ..., Ak, and find the value of S(A1) + S(A2) + ... + S(Ak).

SubsetTests <- function() {
    sum <- 0
    sets <- readLines("resources/p105_sets.txt")
    for (s in sets) {
        set <- as.integer(unlist(strsplit(s,",")))
        if (Special(set)) sum <- sum+sum(set)
    }
    sum
}

Special <- function(A) {
    if (tail(A,1)>sum(A[1:2])) return(FALSE)
    if (any(Reduce(sum,head(A,length(A)-1),accumulate=T)==tail(A,1))) return(FALSE)
    if (any(A[2:(length(A)-1)]+A[1]==tail(A,1))) return(FALSE)
    for (k in 1:(length(A)-1)) {
        all.B <- t(combn(A,k))
        for (i in 1:nrow(all.B)) {
            B <- all.B[i,]
            remaining <- A[!A %in% B]
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

# test
# Optimum(c(81, 88, 75, 42, 87, 84, 86, 65))==FALSE
# Optimum(c(157, 150, 164, 119, 79, 159, 161, 139, 158))==TRUE

a <- SubsetTests()

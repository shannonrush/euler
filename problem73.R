# Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
# 
# If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
#     
#     1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
# 
# It can be seen that there are 3 fractions between 1/3 and 1/2.
# 
# How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?

CountRange <- function(n) {
    a <- 1
    b <- 3
    neighbor <- FindNeighbor(a,b,n)
    p <- neighbor[1]
    q <- neighbor[2]
    count <- 1
    while (p!=1 && q!=2) {
        adj <- FindNext(a,b,p,q,n)
        a <- p
        b <- q
        p <- adj[1]
        q <- adj[2]
        count <- count+1
    }
    count-1
}

FindNext <- function(a,b,c,d,n) {
    f <- floor((n+b)/d)
    p <- f*c-a
    q <- f*d-b
    c(p,q)
}

FindNeighbor <- function(a,b,n) {
    for (q in n:1) {
        for (p in 1:n) {
            if (b*p-1==a*q) return (c(p,q))
        }
    }
}

# test
#CountRange(8)==3

#a <- CountRange(12000)

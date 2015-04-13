# If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
# there are exactly three solutions for p = 120.
# 
# {20,48,52}, {24,45,51}, {30,40,50}
# 
# For which value of p ≤ 1000, is the number of solutions maximised?

IntegerRightTriangle <- function() {
    results <- rep(0,1000)
    for (p in 1:1000) {
        for (a in 1:ceiling(p/4)) {
            for (b in (a+1):((p-a)/2)) {
                c <- p-a-b
                if ((a+b+c)%%2==0) {
                    if (a^2+b^2==c^2) results[a+b+c] <- results[a+b+c]+1     
                } 
            }
        }
    }
    which.max(results)
}

#a<-IntegerRightTriangle()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# IntegerRightTriangle() 44.62608 44.62608 44.62608 44.62608 44.62608 44.62608     1
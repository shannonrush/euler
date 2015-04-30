# The points P (x1, y1) and Q (x2, y2) are plotted at integer co-ordinates and are joined to the origin, O(0,0), 
# to form ΔOPQ.
# 
# 
# There are exactly fourteen triangles containing a right angle that can be formed when each co-ordinate lies between 
# 0 and 2 inclusive; that is,0 ≤ x1, y1, x2, y2 ≤ 2.
# 
# 
# Given that 0 ≤ x1, y1, x2, y2 ≤ 50, how many right triangles can be formed?

RightTriangles <- function(L) {
    count <- L^2*3; # (0,0) is right angle
    for (x in 1:L) {
        for (y in 1:L) {
            d <- gcd(x,y) 
            count <- count + min(floor((y*d)/x), floor(((L-x)*d)/y))*2
        }
    }
    count
}

gcd <- function(x,y) {
    r <- x%%y;
    return(ifelse(r, gcd(y, r), y))
}

# test
#RightTriangles(2)==14
#a <- RightTriangles(50)

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# RightTriangles(50) 42.55015 45.51833 47.48063 46.74234 48.03464 98.59312   100


# By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles. 
# Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.
library(Rcpp)
sourceCpp('problem85.cpp')

CountRectangles <- function() {
    for (a in 1:100) for (b in 1:100) if (abs(2000000-FitRectangles(a,b))<=2) return(a*b)
}


# test
#FitRectangles(3,2)==18
#FitRectangles(4,5)==150
#a <- CountRectangles()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# CountRectangles() 2.138219 2.153622 2.159056 2.159755 2.164345 2.177583    10
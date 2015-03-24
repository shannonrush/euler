# Starting in the top left corner of a 2×2 grid, 
# and only being able to move to the right and down, 
# there are exactly 6 routes to the bottom right corner.
# How many such routes are there through a 20×20 grid?

LatticePaths <- function(n) {
    choose(n+n, n)
}


# test
LatticePaths(2)==6
# TRUE
LatticePaths(20)
# 137846528820

# Unit: microseconds
# expr   min     lq    mean median   uq    max neval
# LatticePaths(20) 1.264 1.3575 2.12558 1.4405 1.63 40.106   100
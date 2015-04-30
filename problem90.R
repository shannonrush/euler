# Each of the six faces on a cube has a different digit (0 to 9) written on it; the same is done to a second cube. 
# By placing the two cubes side-by-side in different positions we can form a variety of 2-digit numbers.
# 
# In fact, by carefully choosing the digits on both cubes it is possible to display all of the square numbers 
# below one-hundred: 01, 04, 09, 16, 25, 36, 49, 64, and 81.
# 
# For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on one cube 
# and {1, 2, 3, 4, 8, 9} on the other cube.
# 
# However, for this problem we shall allow the 6 or 9 to be turned upside-down so that an arrangement 
# like {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for all nine square numbers to be displayed; 
# otherwise it would be impossible to obtain 09.
# 
# In determining a distinct arrangement we are interested in the digits on each cube, not the order.
# 
# {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
# {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}
# 
# But because we are allowing 6 and 9 to be reversed, 
# the two distinct sets in the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} 
# for the purpose of forming 2-digit numbers.
# 
# How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?

CubePairs <- function() {
    combos <- matrix(c(0,0,0,1,2,3,4,6,8,1,4,6,6,5,6,6,4,1),ncol=2)
    count <- 0
    cubes <- t(combn(0:9,6))
    for (a in 1:nrow(cubes)) {
        for (b in 1:nrow(cubes)) {
            if (SquaresDisplayed(cubes[a,],cubes[b,],combos)) count <- count + 1
        }
    }
    count/2 # remove duplicates
}

SquaresDisplayed <- function(a, b, combos) {
    x <- replace(a, which(a==9),6)
    y <- replace(b, which(b==9),6)
    all(apply(combos, 1, HasCombo, x, y))
}

HasCombo <- function(combo, x, y) {
    i <- combo[1]
    j <- combo[2]
    (i %in% x && j %in% y) || (j %in% x && i %in% y)
}

# ans <- CubePairs()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# CubePairs() 6.814465 6.921662 6.966064 6.953819 6.985376 7.230351    10


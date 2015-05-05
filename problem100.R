# If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs, 
# and two discs were taken at random, it can be seen that the probability of taking two blue discs, 
# P(BB) = (15/21)×(14/20) = 1/2.
# 
# The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random, 
# is a box containing eighty-five blue discs and thirty-five red discs.
# 
# By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total, 
# determine the number of blue discs that the box would contain.

options(scipen=999)

ArrangeProb <- function(L) {
    x <- 15
    y <- 21
    while (y<L) {
        x0 <- x
        y0 <- y
        x <- 3*x0 + 2*y0 - 2
        y <- 4*x0 + 3*y0 - 3
    }
    x
}

# test
#ArrangeProb(120)==85

#a <- ArrangeProb(10^12)

# Unit: microseconds
# expr    min    lq    mean median     uq     max neval
# ArrangeProb(10^12) 21.686 22.03 32.8549  23.82 29.234 103.455    10


# notes
#
# start.blue
# start.red
# start.total = start.blue + start.red
# 
# start.blue/start.total * (start.blue-1)/(start.total-1) = 1/2
# 
# (x*(x-1))/(y * (y-1)) = 1/2
# 2x^2 - 2x - y^2 + y = 0
# 
# x1 = 3*x0 + 2*y0 - 2
# y1 = 4*x0 + 3*y0 - 3

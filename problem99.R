# Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator 
# would confirm that 2^11 = 2048 < 3^7 = 2187.
# 
# However, confirming that 632382^518061 > 519432^525806 would be much more difficult, 
# as both numbers contain over three million digits.
# 
# Using base_exp.txt a 22K text file containing one thousand lines with a base/exponent pair on each line, 
# determine which line number has the greatest numerical value.

library(gmp)

LargeExp <- function() {
    max <- 0
    max.i <- 0
    lines <- readLines("resources/p099_base_exp.txt")
    for (i in 1:length(lines)) {
        print(i)
        l <- unlist(strsplit(lines[i],","))
        a <- as.bigz(l[1])
        b <- a^l[2]
        if (b>max) {
            max <- b
            max.i <- i
        }
    }
    max.i
}

#a <- LargeExp()
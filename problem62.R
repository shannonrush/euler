# The cube, 41063625 (345^3), can be permuted to produce two other cubes: 
#     56623104 (384^3) and 66430125 (405^3). 
# 
# In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
# 
# Find the smallest cube for which exactly five permutations of its digits are cube.

options(scipen=999)

CubicPerms <- function(n) {
    perms <- data.frame(max=c(), times=c(), min=c())
    i <- 1
    while (nrow(perms[perms$times==n,])==0) {
        cube <- i^3
        max <- as.numeric(paste(sort(as.numeric(unlist(strsplit(toString(cube),""))),decreasing=T),collapse=""))
        if (max %in% perms$max) {
            row <- perms[perms$max==max,]
            times <- row$times + 1
            min <- ifelse(cube<row$min, cube, row$min)
            perms[perms$max==max,] <- c(max, times, min)
        } else {
            perms <- rbind(perms, data.frame(max=max, times=1, min=cube))
        }
        i <- i + 1
    }
    perms[perms$times==n,"min"]
}

# test 
#CubicPerms(3)==41063625

#a <- CubicPerms(5)

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# CubicPerms(5) 14.54825 14.76263 15.10779 15.01644 15.43543 15.92813    10

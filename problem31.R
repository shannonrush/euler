# In England the currency is made up of pound, £, and pence, p, 
# and there are eight coins in general circulation:
#     
#     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
# 
#    It is possible to make £2 in the following way:
#     
#     1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
#
# How many different ways can £2 be made using any number of coins?

CoinSums <- function() {
    coins <- c(200,100,50,20,10,5,2,1)
    NumCombos(0, coins, 1)
}

NumCombos <- function(sum, coins, i) {
    if (sum==200) {
        return(1)
    } else if (sum>200) {
        return(0)
    } else {
        a <- NumCombos(sum+coins[i], coins, i)
        b <- ifelse(i+1<=length(coins), NumCombos(sum, coins, i+1),0)
        a+b
    }
}

# Unit: seconds
# expr     min       lq     mean  median       uq     max neval
# CoinSums() 14.2265 14.28216 14.29105 14.2941 14.32182 14.3289    10
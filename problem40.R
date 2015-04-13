# An irrational decimal fraction is created by concatenating the positive integers:
#     
#     0.123456789101112131415161718192021...
# 
# It can be seen that the 12th digit of the fractional part is 1.
# 
# If dn represents the nth digit of the fractional part, find the value of the following expression.
# 
# d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

ChampCons <- function() {
    dec <- paste(1:1000000,collapse="")
    prod(as.integer(sapply(c(1,10,100,1000,10000,100000,1000000), Nth, dec)))
}

Nth <- function(i, dec) {
    substr(dec, i, i)
}

# test
#Nth(12, paste(1:21,collapse=""))==1

#ChampCons()

# Unit: milliseconds
# expr      min       lq    mean   median       uq      max neval
# ChampCons() 606.1288 667.7741 713.838 702.4228 748.2224 967.9375   100
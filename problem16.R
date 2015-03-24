# 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
# 
# What is the sum of the digits of the number 2^1000?

PowerDigitSum <- function(n) {
    options(scipen=999)
    sum(as.integer(unlist(strsplit(toString(2^n),""))))
}

# test
PowerDigitSum(15)==26
PowerDigitSum(1000)
#[1] 1366

# Unit: microseconds
# expr    min     lq     mean median     uq    max neval
# PowerDigitSum(1000) 88.971 89.838 91.96253 90.346 90.869 152.35   100


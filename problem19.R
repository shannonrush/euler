# You are given the following information, but you may prefer to do some research for yourself.
# 
# 1 Jan 1900 was a Monday.
# Thirty days has September,
# April, June and November.
# All the rest have thirty-one,
# Saving February alone,
# Which has twenty-eight, rain or shine.
# And on leap years, twenty-nine.
# A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
# 
# How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

CountingSundays <- function() {
    sum(sapply(1901:2000, function(Y) sapply(1:12, function(m) DateToDay(Y, sprintf("%02s",m))))=="Sunday")
}

DateToDay <- function(Y,m,d) {
    format(as.Date(paste0(Y,m,"01"), format="%Y%m%d", tz="UTC"), "%A")
}

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# CountingSundays() 83.74765 86.74091 88.09762 87.05492 87.81078 149.2398   100


# The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
#     
#     1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
# 
# By converting each letter in a word to a number corresponding to its 
# alphabetical position and adding these values we form a word value. 
# 
# For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. 
# If the word value is a triangle number then we shall call the word a triangle word.
# 
# Using words.txt a 16K text file containing nearly two-thousand common English words, how many are triangle words?

CodedTriangleNum <- function() {
    words <- tolower(as.character(read.table("resources/p042_words.txt", sep=",", stringsAsFactors=F)))
    max.chars <- max(nchar(words))
    triangle.nums <- sapply(1:(max.chars*26), function(n) (0.5*n)*(n+1))
    sum(sapply(words, IsTriangle, triangle.nums))
}

IsTriangle <- function(word, triangle.nums) {
    word.value <- sum(sapply(1:nchar(word), function(i) which(letters==substr(word,i,i))))
    ifelse(word.value %in% triangle.nums, 1, 0)
}

# test
# IsTriangle("sky", sapply(1:(max.chars*26), function(n) (0.5*n)*(n+1)))==1

# a <- CodedTriangleNum()

# Unit: milliseconds
# expr      min      lq     mean   median       uq      max neval
# CodedTriangleNum() 262.2257 274.427 336.0807 340.1432 389.7424 446.5344   100
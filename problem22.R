# Using names.txt, a 46K text file containing over five-thousand first names, 
# begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, 
# multiply this value by its alphabetical position in the list to obtain a name score.
# 
# For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, 
# is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
# 
# What is the total of all the name scores in the file?

NameScores <- function() {
    names <- sort(as.character(read.table("resources/p022_names.txt", sep=",", stringsAsFactors=F)))
    sum(sapply(names, function(name) AlphaScore(name) * which(names==name)))
}

AlphaScore <- function(name) {
    chars <- tolower(unlist(strsplit(name,"")))
    sum(sapply(chars, function(char) which(letters==char)))
}

# test
# AlphaScore("COLIN")==53
# 
# NameScores()
# 871198282

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# NameScores() 1.939899 1.992359 2.034929 2.029901 2.054835 2.295415   100

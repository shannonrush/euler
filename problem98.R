# By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, 
# we form a square number: 1296 = 36^2. 
# 
# What is remarkable is that, by using the same digital substitutions, the anagram, RACE, 
# also forms a square number: 9216 = 96^2. 
# 
# We shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted, 
# neither may a different letter have the same digital value as another letter.
# 
# Using words.txt, a 16K text file containing nearly two-thousand common English words, 
# find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).
# 
# What is the largest square number formed by any member of such a pair?
# 
# NOTE: All anagrams formed must be contained in the given text file.

library(combinat)

MaxAnagram <- function() {
    words <- WordsDF()
    max <- 0
    while (max==0) {
        letters <- words[1,2]
        anagrams <- words[words$sorted==letters,"words"]
        max <- MaxSquare(anagrams)
        words <- words[words$sorted!=letters,]
    }
    words <- words[words$nchar==nchar(letters),]
    while (nrow(words)>0) {
        letters <- words[1,2]
        anagrams <- words[words$sorted==letters,"words"]
        words <- words[words$sorted!=letters,]
        max.square <- MaxSquare(anagrams)
        if (max.square > max) max <- max.square
    }
    max
}

MaxSquare <- function(anagrams) {
    word <- anagrams[1]
    anagram <- anagrams[2]
    letters <- unlist(strsplit(anagram,""))
    pairs <- c()
    combos <- t(combn(0:9,nchar(word)))
    dict <- data.frame(letters=unlist(strsplit(word,"")),digits=rep(NA,nchar(word)))
    for (i in 1:nrow(combos)) {
        combo <- combos[i,]
        perms <- matrix(unlist(permn(combo)), ncol=ncol(combos), byrow=T)
        for (j in 1:nrow(perms)) {
            if (perms[j,1]!=0) {
                n <- as.integer(paste(perms[j,],collapse=""))
                if (IsSquare(n)) {
                    dict$digits <- as.integer(unlist(strsplit(toString(n),"")))
                    n2 <- as.integer(paste(sapply(letters, function(x) dict[dict$letters==x,"digits"]),collapse=""))
                    if (nchar(n2)==nchar(word)) if (IsSquare(n2)) pairs <- c(pairs, n, n2)
                }
            }
        }
    }
    ifelse(length(pairs)>0,max(pairs),0)
}

WordsDF <- function() {
    words <- as.character(read.table("resources/p098_words.txt", stringsAsFactors=F, sep=","))
    sorted <- sapply(words, SortLetters)
    nchar <- sapply(words, nchar)
    d <- data.frame(words, sorted, nchar, stringsAsFactors=F, row.names=NULL)
    t <- table(d$sorted)
    anagrams <- names(t[t>1])   
    d <- d[d$sorted %in% anagrams,]
    d[with(d, order(nchar, decreasing=T)),]
}

SortLetters <- function(word) {
    paste(sort(unlist(strsplit(word,""))),collapse="")
}

IsSquare <- function(n) sqrt(n)%%1==0

#a <- MaxAnagram()

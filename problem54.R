# In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, 
# in the following way:
#     
# High Card: Highest value card.
# One Pair: Two cards of the same value.
# Two Pairs: Two different pairs.
# Three of a Kind: Three cards of the same value.
# Straight: All cards are consecutive values.
# Flush: All cards of the same suit.
# Full House: Three of a kind and a pair.
# Four of a Kind: Four cards of the same value.
# Straight Flush: All cards are consecutive values of same suit.
# Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
# The cards are valued in the order:
#     2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
# 
# If two players have the same ranked hands then the rank made up of the highest value wins; 
# for example, a pair of eights beats a pair of fives (see example 1 below). 
# But if two ranks tie, for example, both players have a pair of queens, 
# then highest cards in each hand are compared (see example 4 below); 
# if the highest cards tie then the next highest cards are compared, and so on.
# 
# The file, poker.txt, contains one-thousand random hands dealt to two players. 
# Each line of the file contains ten cards (separated by a single space): 
# the first five are Player 1's cards and the last five are Player 2's cards. 
# You can assume that all hands are valid (no invalid characters or repeated cards), 
# each player's hand is in no specific order, and in each hand there is a clear winner.
# 
# How many hands does Player 1 win?

Poker <- function() {
    hands <- as.matrix(read.table("resources/p054_poker.txt", stringsAsFactors = F))
    sum(apply(hands, 1, P1Wins))
}

P1Wins <- function(hand) {
    p1 <- hand[1:5]
    p2 <- hand[6:10]
    royal.flush <- HasRoyalFlush(p1,p2)
    if (length(royal.flush)==1) return(royal.flush)
    straight.flush <- HasStraightFlush(p1,p2)
    if (length(straight.flush)==1) return(straight.flush)
    four.kind <- HasFourKind(p1, p2)
    if (length(four.kind)==1) return(four.kind)
    full.house <- HasFullHouse(p1,p2)
    if (length(full.house)==1) return(full.house)
    flush <- HasFlush(p1,p2)
    if (length(flush)==1) return(flush)
    straight <- HasStraight(p1,p2)
    if (length(straight)==1) return(straight)
    three.kind <- HasThreeKind(p1,p2)
    if (length(three.kind)==1) return(three.kind)
    two.pairs <- HasTwoPairs(p1,p2)
    if (length(two.pairs)==1) return(two.pairs)
    one.pair <- HasOnePair(p1,p2)
    if (length(one.pair)==1) return(one.pair)
    HasHighCard(p1,p2)
}

HasRoyalFlush <- function(p1,p2) {
    # return 1 if p1, 0 if p2, c(1,0) if both
    has <- c()
    if (RoyalFlush(p1)) has <- c(has, 1)
    if (RoyalFlush(p2)) has <- c(has, 0)
    has
}

RoyalFlush <- function(hand) {
    if (identical(sort(substr(hand,1,1)),c("A","J","K","Q","T"))) {
        if (length(unique(substr(hand,2,2)))==1) return(TRUE)
    }
    FALSE
}

HasStraightFlush <- function(p1,p2) {
    has <- c()
    if (StraightFlush(p1)) has <- c(has, 1)
    if (StraightFlush(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        rank1 <- CardSort(substr(p1,1,1))[5]
        rank2 <- CardSort(substr(p2,1,1))[5]
        if (rank1==rank2) {
            has <- (c(0,1)) 
        } else {
            has <- ifelse(which(consec==rank1)>which(consec==rank2), 1, 0)
        }
    }
    has
}

StraightFlush <- function(hand) {
    consec <- (c(as.character(2:9),"T","J","Q","K","A"))
    sorted <- CardSort(substr(hand,1,1))
    i <- which(consec==sorted[1])
    suits <- substr(hand,2,2)
    identical(sorted, consec[i:(i+4)]) && length(unique(suits))==1
}

HasFourKind <- function(p1, p2) {
    has <- c()
    if (FourKind(p1)) has <- c(has, 1)
    if (FourKind(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        t1 <- table(substr(p1,1,1))
        rank1 <- names(t1[t1==4])
        t2 <- table(substr(p2,1,1))
        rank2 <- names(t2[t2==4])
        has <- ifelse(which(consec==rank1)>which(consec==rank2), 1, 0)
    }
    has
}

FourKind <- function(hand) {
    ranks <- substr(hand,1,1)
    any(table(ranks)==4)
}

HasFullHouse <- function(p1,p2) {
    has <- c()
    if (FullHouse(p1)) has <- c(has, 1)
    if (FullHouse(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        t1 <- table(substr(p1,1,1))
        rank1 <- names(t1[t1==3])
        t2 <- table(substr(p2,1,1))
        rank2 <- names(t2[t2==3])
        has <- ifelse(which(consec==rank1)>which(consec==rank2), 1, 0)
    }
    has
}

FullHouse <- function(hand) {
    ranks <- substr(hand,1,1)
    t <- table(ranks)
    3%in%t && 2%in%t
}

HasFlush <- function(p1,p2) {
    has <- c()
    if (Flush(p1)) has <- c(has, 1)
    if (Flush(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        ranks1 <- CardSort(substr(p1,1,1))
        ranks2 <- CardSort(substr(p2,1,1))
        for (i in 1:5) {
            if (ranks1[i]!=ranks2[i]) has <- ifelse(which(consec==ranks1[i])>which(consec==ranks2[i]), 1, 0)
        }
    }
    has
}

Flush <- function(hand) {
    suits <- substr(hand,2,2)
    length(unique(suits))==1
}

HasStraight <- function(p1,p2) {
    has <- c()
    if (Straight(p1)) has <- c(has, 1)
    if (Straight(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        rank1 <- CardSort(substr(p1,1,1))[5]
        rank2 <- CardSort(substr(p2,1,1))[5]
        if (rank1==rank2) {
            has <- (c(0,1)) 
        } else {
            has <- ifelse(which(consec==rank1)>which(consec==rank2), 1, 0)
        }
    }
    has
}

Straight <- function(hand) {
    consec <- (c(as.character(2:9),"T","J","Q","K","A"))
    sorted <- CardSort(substr(hand,1,1))
    i <- which(consec==sorted[1])
    identical(sorted, consec[i:(i+4)]) 
}

HasThreeKind <- function(p1, p2) {
    has <- c()
    if (ThreeKind(p1)) has <- c(has, 1)
    if (ThreeKind(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        t1 <- table(substr(p1,1,1))
        rank1 <- names(t1[t1==3])
        t2 <- table(substr(p2,1,1))
        rank2 <- names(t2[t2==3])
        has <- ifelse(which(consec==rank1)>which(consec==rank2), 1, 0)
    }
    has
}

ThreeKind <- function(hand) {
    ranks <- substr(hand,1,1)
    any(table(ranks)==3)
}

HasTwoPairs <- function(p1,p2) {
    has <- c()
    if (TwoPairs(p1)) has <- c(has, 1)
    if (TwoPairs(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        ranks1 <- substr(p1,1,1)
        ranks2 <- substr(p2,2,2)
        pairs1 <- names(which(table(ranks1)==2))
        pairs2 <- names(which(table(ranks2)==2))
        hp1 <- max(sapply(pairs1, function(p) which(consec==p)))
        hp2 <- max(sapply(pairs2, function(p) which(consec==p)))
        if (hp1 != hp2) {
            has <- ifelse(hp1>hp2,1,0)  
        } else {
            lp1 <- min(sapply(pairs1, function(p) which(consec==p)))
            lp2 <- min(sapply(pairs2, function(p) which(consec==p)))
            if (lp1 != lp2) {
                has <- ifelse(lp1>lp2,1,0)
            } else {
                r1 <- names(which(table(ranks1)==1))
                r2 <- names(which(table(ranks2)==1))
                if (r1 != r2) has <- ifelse(r1>r2,1,0)
            }
        }
    }
    has
}

TwoPairs <- function(hand) {
    ranks <- substr(hand,1,1)
    sum(table(ranks)==2)==2
}

HasOnePair <- function(p1,p2) {
    has <- c()
    if (OnePair(p1)) has <- c(has, 1)
    if (OnePair(p2)) has <- c(has, 0)
    if (length(has)==2) {
        consec <- (c(as.character(2:9),"T","J","Q","K","A"))
        ranks1 <- substr(p1,1,1)
        ranks2 <- substr(p2,1,1)
        pair1 <- names(which(table(ranks1)==2))
        pair2 <- names(which(table(ranks2)==2))
        r1 <- which(consec==pair1)
        r2 <- which(consec==pair2)
        if (r1 != r2) {
            has <- ifelse(r1>r2,1,0)
        } else {
            p <- pair1
            sort1 <- CardSort(ranks1[-which(ranks1==p)])
            sort2 <- CardSort(ranks2[-which(ranks2==p)])
            for (i in 1:3) {
                if (sort1[i]!=sort2[i]) has <- ifelse(sort1[i]>sort2[i],1,0)
            }
        }
    }
    has
}

OnePair <- function(hand) {
    ranks <- substr(hand,1,1)
    any(table(ranks)==2)
}

HasHighCard <- function(p1,p2) {
    consec <- (c(as.character(2:9),"T","J","Q","K","A"))
    ranks1 <- rev(CardSort(substr(p1,1,1)))
    ranks2 <- rev(CardSort(substr(p2,1,1)))
    for (i in 1:5) {
        if (ranks1[i]!=ranks2[i]) {
            r1 <- which(consec==ranks1[i])
            r2 <- which(consec==ranks2[i])
            return(ifelse(r1>r2,1,0))
        }
    }
}

CardSort <- function(ranks) {
    consec <- (c(as.character(2:9),"T","J","Q","K","A"))
    sorted <- c()
    for (c in consec) {
        while (c %in% ranks) {
            sorted <- c(sorted, c)
            ranks <- ranks[-which(ranks==c)[1]]
        }
    }
    sorted
}

# test
# RoyalFlush(c("TH","KH","QH","JH","AH"))==TRUE
# RoyalFlush(c("TH","KH","QH","JH","AC"))==FALSE
# StraightFlush(c("2H","4C","3C","5D","6H"))==TRUE
# StraightFlush(c("TH","9H","8H","7H","6H"))==TRUE
# StraightFlush(c("TH","9H","8H","7H","6D"))==FALSE
# StraightFlush(c("TH","9H","5H","7H","6H"))==FALSE
# FourKind(c("AH","AD","AS","AC","5H"))==TRUE
# FourKind(c("AH","AD","AS","4C","5H"))==FALSE
# FullHouse(c("AH","AD","AS","4C","4H"))==TRUE
# FullHouse(c("AH","AD","AS","4C","5H"))==FALSE
# Flush(c("AH","JH","TH","5H","4H"))==TRUE
# Flush(c("AH","JH","TH","5H","4D"))==FALSE
# Straight(c("TH","9H","8H","7H","6D"))==TRUE
# Straight(c("TH","9H","8H","7H","5D"))==FALSE
# Straight(c("6H", "4H", "5C", "3H", "2H"))==TRUE
# Straight(c("3S", "QH", "5S", "6S", "AS"))==FALSE
# ThreeKind(c("AH","AD","AS","4C","5H"))==TRUE
# ThreeKind(c("AH","AD","4S","4C","5H"))==FALSE
# TwoPairs(c("AH","AD","4S","4C","5H"))==TRUE
# TwoPairs(c("AH","AD","4S","6C","5H"))==FALSE
# OnePair(c("AH","AD","4S","4C","5H"))==TRUE
# OnePair(c("AH","AD","4S","6C","5H"))==TRUE
# OnePair(c("AH","KD","3S","4C","5H"))==FALSE
# P1Wins(c("5H", "5C", "6S", "7S", "KD", "2C", "3S", "8S", "8D", "TD"))==0
# P1Wins(c("5D", "8C","9S", "JS", "AC", "2C", "5C", "7D", "8S", "QH"))==1
# P1Wins(c("2D","9C", "AS", "AH", "AC", "3D", "6D", "7D", "TD", "QD"))==0
# P1Wins(c("4D","6S","9H","QH","QC","3D","6D","7H","QD","QS"))==1
# P1Wins(c("2H","2D","4C","4D","4S","3C","3D","3S","9S","9D"))==1
# P1Wins(c("8C", "TS", "KC", "9H", "4S","7D", "2S", "5D", "3S", "AC"))==0
# P1Wins(c("6H", "4H", "5C", "3H", "2H", "3S", "QH", "5S", "6S", "AS"))==1
# HasStraight(c("6H", "4H", "5C", "3H", "2H"), c("3S", "QH", "5S", "6S", "AS"))==1

#a <- Poker()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# Poker() 2.404421 2.422765 2.459356 2.462143 2.495593 2.513949    10

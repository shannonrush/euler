# problem statement is very long, read it here: https://projecteuler.net/problem=84

Monopoly <- function(die) {
    squares <- sapply(0:39, function(x) sprintf("%02s", x))
    CC <- c("02","17","33")
    CH <- c("07","22","36")
    CC.cards <- sample(c(rep("",14),"00","10"))
    CH.cards <- sample(c(rep("",6),"00","10","11","24","39","05","R","R","U","-3"))
    visited <- c()
    current <- "00"
    doubles <- 0
    for (i in 1:500000) {
        roll <- RollDice(die)
        doubles <- ifelse(roll[1]==roll[2], doubles + 1, 0)
        if (doubles==3) {
            current <- "10"
            doubles <- 0
        } else {
            current <- Move(current, roll)
            if (current=="30") {
                current <- "10"
            } else if (current %in% CC) {
                card <- CC.cards[1]
                CC.cards <- c(CC.cards[-1],card)
                current <- ifelse(card=="", current, card)
            } else if (current %in% CH) {
                card <- CH.cards[1]
                CH.cards <- c(CH.cards[-1],card)
                if (card %in% c(squares,"")) {
                    current <- ifelse(card=="", current, card)
                } else {
                    if (card=="R") {
                        current <- NextRR(current)
                    } else if (card=="U") {
                        current <- NextU(current)
                    } else if (card=="-3") {
                        current <- Move(current, -3)
                    }
                }
            }
        }
        visited <- c(visited, current)
    }
    ThreeMost(visited)
}

ThreeMost <- function(visited) {
    paste(names(sort(table(visited),decreasing=T)[1:3]),collapse="")
}

RollDice <- function(die) c(sample(die,1),sample(die,1))

Move <- function(current, roll) {
    sprintf("%02s",(as.integer(current)+sum(roll))%%40)
}

NextRR <- function(current) {
    if (current=="07") {
        return("10")
    } else if (current=="22") {
        return("25")
    } else if (current=="36") {
        return("05")
    }
}

NextU <- function(current) {
    if (current %in% c("07","36")) {
        return("12")
    } else if (current=="22") {
        return("28")
    } 
}

# test

# Monopoly(6)=="102400"

a<-Monopoly(4)

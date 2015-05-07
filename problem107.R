# It is possible to optimise the network by removing some edges and still ensure that all points on the network remain connected.
# The network which achieves the maximum saving is shown below. It has a weight of 93, representing a saving of 243 − 93 = 150 
# from the original network.
# 
# Using network.txt, a 6K text file containing a network with forty vertices, and given in matrix form, find the maximum saving 
# which can be achieved by removing redundant edges whilst ensuring that the network remains connected.

library(vegan)

MinNetwork <- function() {
    M <- as.matrix(read.table("resources/p107_network.txt",sep=",",na.strings="-"))
    orig.sum <- sum(M,na.rm=T)/2
    orig.sum-sum(spantree(M)$dist)
}

# test
#M <- matrix(c(NA,16,12,21,NA,NA,NA,16,NA,NA,17,20,NA,NA,12,NA,NA,28,NA,31,NA,21,17,28,NA,18,19,23,NA,20,NA,18,NA,NA,11,NA,NA,31,19,NA,NA,27,NA,NA,NA,23,11,27,NA),ncol=7,nrow=7,byrow=T)
#MinNetwork(M)==150
#a <- MinNetwork()

# Unit: milliseconds
# expr     min      lq    mean  median      uq     max neval
# MinNetwork() 48.1699 49.5539 51.8691 50.0624 50.6375 180.775   100



# Three distinct points are plotted at random on a Cartesian plane, for which -1000 ≤ x, y ≤ 1000, 
# such that a triangle is formed.
# 
# Consider the following two triangles:
#     
# A(-340,495), B(-153,-910), C(835,-947)
# X(-175,41), Y(-421,-714), Z(574,-645)
# 
# It can be verified that triangle ABC contains the origin, whereas triangle XYZ does not.
# 
# Using triangles.txt, a 27K text file containing the co-ordinates of one thousand "random" triangles, 
# find the number of triangles for which the interior contains the origin.

TriangleContainment <- function() {
    triangles <- readLines("resources/p102_triangles.txt")
    sum(sapply(triangles, IsContained))
}

IsContained <- function(tri.string) {
    points <- as.integer(unlist(strsplit(tri.string,",")))
    A <- points[1:2]
    B <- points[3:4]    
    C <- points[5:6]
    P <- c(0,0)
    v0 <- C-A
    v1 <- B-A
    v2 <- P-A
    dot00 <- v0 %*% v0
    dot01 <- v0 %*% v1
    dot02 <- v0 %*% v2
    dot11 <- v1 %*% v1
    dot12 <- v1 %*% v2
    invD <- 1/(dot00 * dot11 - dot01 * dot01)
    u <- (dot11 * dot02 - dot01 * dot12) * invD
    v <- (dot00 * dot12 - dot01 * dot02) * invD
    (u >= 0) && (v >= 0) && (u + v < 1)
}

# test
#IsContained("-340,495,-153,-910,835,-947")==TRUE
#IsContained("-175,41,-421,-714,574,-645")==FALSE

#a <- TriangleContainment()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# TriangleContainment() 139.4415 141.2085 143.7519 142.1619 143.5843 201.8706   100

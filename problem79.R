# A common security method used for online banking is to 
# ask the user for three random characters from a passcode. 
# 
# For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; 
# the expected reply would be: 317.
# 
# The text file, keylog.txt, contains fifty successful login attempts.
# 
# Given that the three characters are always asked for in order, 
# analyse the file so as to determine the shortest possible secret passcode of unknown length.

Passcode <- function() {
    attempts <- readLines("resources/p079_keylog.txt")
    code <- c()
    for (attempt in attempts) {
        digits <- unlist(strsplit(attempt,""))
        for (digit in digits) {
            if (!digit %in% code) {
                code <- c(code, digit)
            } else {
                remaining <- setdiff(digits, digit)
                for (r in remaining) {
                    if (r %in% code) {
                        if (which(digits==digit)<which(digits==r) && which(code==digit)>which(code==r)) {
                            code <- code[-which(code==digit)]
                            code <- append(code, digit, after=which(code==r)-1)
                        } else if (which(digits==digit)>which(digits==r) && which(code==digit)<which(code==r)) {
                            code <- code[-which(code==digit)]
                            code <- append(code, digit, after=which(code==r))
                        }
                    }
                }
            }
        }
    }
    paste(code,collapse="")
}

a <- Passcode()

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval
# Passcode() 6.226147 6.533736 7.541641 6.869586 7.585659 48.22336   100
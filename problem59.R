# Each character on a computer is assigned a unique code and the preferred standard is ASCII. 
# For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
# 
# A modern encryption method is to take a text file, convert the bytes to ASCII, 
# then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same 
# encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
# 
# For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. 
# The user would keep the encrypted message and the encryption key in different locations, and without both "halves", 
# it is impossible to decrypt the message.
# 
# Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. 
# If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. 
# The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
# 
# Your task has been made easy, as the encryption key consists of three lower case characters. 
# Using cipher.txt, a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain 
# common English words, decrypt the message and find the sum of the ASCII values in the original text.

XORDecrypt <- function() {
    encrypted <- as.integer(read.table("resources/p059_cipher.txt",sep=","))
    words <- readLines("resources/4000-most-common-english-words-csv.csv")
    candidates <- apply(combn(letters, 3), 2, function(x) sapply(x, CharToAscii))
    for (i in 1:ncol(candidates)) {
        decrypted <- Decrypt(encrypted, candidates[,i])
        if (sum(sapply(words, grepl, decrypted)) > 100) return(sum(sapply(unlist(strsplit(decrypted,"")), CharToAscii)))
    }    
}

Decrypt <- function(encrypted, key) {
    msg <- sapply(1:length(encrypted), function(i) bitwXor(encrypted[i], key[(i%%3)+1]))
    paste(sapply(msg, AsciiToChar), collapse="")
}

CharToAscii <- function(x) { 
    strtoi(charToRaw(x),16L) 
}

AsciiToChar <- function(n) { 
    rawToChar(as.raw(n)) 
}

#a <- XORDecrypt()

# Unit: seconds
# expr      min       lq     mean   median       uq      max neval
# XORDecrypt() 150.7087 150.7087 150.7087 150.7087 150.7087 150.7087     1
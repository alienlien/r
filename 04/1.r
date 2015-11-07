findWords <- function(tf) {
    txtArray <- scan(tf, "")
    out <- list()
    for (i in 1:length(txtArray)) {
        word <- txtArray[i]
        out[[word]] <- c(out[[word]], i)
    }
    return(out)
}

getSortedWordList <- function(inList) {
    return(inList[sort(names(inList))])
}

getSortedWordListFreq <- function(inList) {
    return(inList[order(sapply(inList, length), decreasing=TRUE)])
}

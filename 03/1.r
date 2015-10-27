blurpart <- function(img, rows, cols, q) {
    numRows <- length(rows)
    numCols <- length(cols)
    newIMG <- img
    randomNoise <- matrix(nrow = numRows, ncol = numCols, runif(numRows * numCols))
    newIMG@grey[rows, cols] <- (1-q) * img@grey[rows, cols] + q * randomNoise
    return(newIMG)
}

makecov <- function(rho, n) {
    m <- matrix(nrow = n, ncol = n)
    m <- ifelse(row(m) == col(m), 1, rho)
    return(m)
}

getMaj <- function(row, dim) {
    maj <- sum(row[1:dim])/dim
    return(if (maj > 0.5) 1 else 0)
}

findols <- function(x) {
    findol <- function(xrow) {
        mdn <- median(xrow)
        devs <- abs(xrow - mdn)
        return(which.max(devs))
    }
    return(apply(x, 1, findol))
}

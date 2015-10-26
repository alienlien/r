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

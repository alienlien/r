getMinInMatrix <- function(m) {
    minVal <- min(m[row(m) != col(m)])
    indices <- which(m == minVal, arr.ind=TRUE)
    return(matrix(indices[indices[,2] > indices[,1]], ncol=2))
}

getMinInRowFromIndex <- function(row, begin) {
    return(getMinInRowWithIndices(row, begin:length(row)))
}

getMinInRowWithIndices <- function(row, indices) {
    idxMin <- which.min(row[indices])
    idxMinInRow <- indices[idxMin]
    return(c(idxMinInRow, row[idxMinInRow]))
}

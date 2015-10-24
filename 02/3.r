findud <- function(v) {
    vud <- v[-1] - v[-length(v)]
    return(ifelse(vud > 0, 1, -1))
}

udcorr <- function(x, y) {
    ud <- lapply(list(x, y), findud)
    return(mean(ud[[1]] == ud[[2]]))
}

udcorr2 <- function(x,y) {
    return(mean(sign(diff(x))==sign(diff(y))))
}

f <- function(x, c) {
    if (length(c) > 1) {
        stop("C can be a scalar only!!")
    }
    return((x+c)^2)
}

g <- function(x) {
    return(c(x, x^2))
}

h <- function(v) {
    z <- NULL
    for (x in v) {
        if (x %% 2 == 0) {
            z = c(z, x)
        }
    }
    return(z)
}

f1 <- function(x) {
    return(which(x == 1)[1])
}

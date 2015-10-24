first1 <- function(x) {
    for (i in 1:length(x)) {
        if (x[i] == 1) {
            break
        }
    }
    return(i)
}

# findRuns returns the starting points of consecutive runs of 1 in the vector x.
findRuns <- function(x, k) {
    n <- length(x)
    runs <- NULL
    for (i in 1:(n-k+1)) {
        if (all(x[i:(i+k-1)]==1)) {
            runs <- c(runs, i)
        }
    }
    return(runs)
}

# findRuns2 returns the starting points of consecutive runs of 1 in the vector x.
findRuns2 <- function(x, k) {
    n <- length(x)
    runs <- vector(length=n)
    count <- 0
    for (i in 1:(n-k+1)) {
        if (all(x[i:(i+k-1)]==1)) {
            count <- count + 1
            runs[count] = i   
        }
    }
    if (count > 0) {
        return(runs[1:count])
    }
    return(NULL)
}

preda <- function(x, k) {
    n <- length(x)
    k2 <- k/2

    pred <- vector(length=n-k)
    for (i in 1:(n-k)) {
        if (sum(x[i:(i+k-1)]) >= k2) {
            pred[i] <- 1
        } else {
            pred[i] <- 0
        }
    }
    return(mean(abs(pred-x[(k+1):n])))
}

predc <- function(x, k) {
    n <- length(x)
    k2 <- k/2

    pred <- vector(length=n-k)
    csx <- c(0, cumsum(x))
    for (i in 1:(n-k)) {
        if ((csx[i+k] - csx[i]) >= k2) {
            pred[i] <- 1
        } else {
            pred[i] <- 0
        }
    }
    return(mean(abs(pred-x[(k+1):n])))
}

# # predWithPercent returns the error rate of the prediction.
# # - data   : The data of all the raining days.
# # - numPast: Number of days in past used to predict.
# # - precent: Percentage of the raining days to predict raining.
# predWithPercent <- function(data, numPast, percent) {
#     numData <- length(data)
#     numPredict <- numData-numPast
#     pred <- vector(length=numPredict)
#     for (i in 1:numPredict) {
#         if (willRain(data[i:(i+numPast-1)], percent)) {
#             pred[i] <- 1
#         } else {
#             pred[i] <- 0
#         }
#     }
#     print(pred)
#     return(mean(abs(pred-data[(numPast+1):numData])))
# }
# 
# # willRain predicts whether it rains or not by using the past data.
# willRain <- function(past, percent) {
#     bound <- floor(length(past) * percent)
#     print(sprintf("Past: %s, Sum of past: %s, Bound: %s\n", past, sum(past), bound))
#     return(sum(past) >= bound)
# }
# 

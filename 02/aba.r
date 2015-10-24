aba <- read.csv("../data/Abalone.data", header=TRUE, as.is=TRUE)
grps <- list()
for (gen in c("M", "F")) {
    grps[[gen]] <- which(aba[, 1]==gen)
}
abam <- aba[grps$M, ]
abaf <- aba[grps$F, ]
plot(abam$Length, abam$Diameter)
plot(abaf$Length, abaf$Diameter, pch="x", new=FALSE)

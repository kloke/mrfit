mrdisp <-
function (x, y, center, beta) 
{
    a <- unique(center)
    nc <- length(a)
    Dvec <- rep(0, nc)
    for (j in 1:nc) {
        x1 <- as.matrix(x[center == a[j], ])
        y1 <- y[center == a[j]]
        nj <- length(y1)
        e <- y1 - x1 %*% beta
        sj <- as.matrix(sqrt(12) * (rank(e, ties.method = "random")/(nj + 
            1) - 0.5))
        Dvec[j] <- t(sj) %*% e
    }
    sum(Dvec)
}

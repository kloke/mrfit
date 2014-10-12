mrgrad <-
function (x, y, center, beta) 
{
    p <- ncol(x)
    a <- unique(center)
    nc <- length(a)
    Smat <- matrix(nrow = p, ncol = nc)
    Dvec <- rep(0, nc)
    beta <- as.matrix(beta)
    for (j in 1:nc) {
        x1 <- as.matrix(x[center == a[j], ])
        y1 <- y[center == a[j]]
        nj <- length(y1)
        e <- y1 - x1 %*% beta
        sj <- as.matrix(sqrt(12) * (rank(e, ties.method = "random")/(nj + 
            1) - 0.5))
        Smat[, j] <- t(x1) %*% sj
    }
    apply(Smat, 1, sum)
}

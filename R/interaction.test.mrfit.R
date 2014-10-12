interaction.test.mrfit <-
function (x, y, block) 
{
    ub <- unique(block)
    m <- length(ub)
    p <- ncol(x)
    bigbetahat <- vector(mode = "numeric", length = m * p)
    tauhat <- vector(mode = "numeric", length = m)
    one <- rep(1, m - 1)
    Ip <- diag(rep(1, p))
    H1 <- one %x% Ip
    H2 <- -diag(1 * one) %x% Ip
    H <- cbind(H1, H2)
    V <- matrix(0, nrow = m * p, ncol = m * p)
    min.n <- sum(block == ub[1])
    for (i in 1:m) {
        yi <- y[block == ub[i]]
        xi <- x[block == ub[i], ]
        xi <- xi - outer(rep(1, nrow(xi)), apply(xi, 2, mean))
        if (length(yi) < min.n) {
            min.n <- length(yi)
        }
        fit <- rfit(xi, yi)
        bigbetahat[((i - 1) * p + 1):(i * p)] <- fit$coef
        V[((i - 1) * p + 1):(i * p), ((i - 1) * p + 1):(i * p)] <- solve(crossprod(xi))
    }
    tauhat <- mrfit(x, y, block)$tauhat
    q <- nrow(H)
    HB <- H %*% bigbetahat
    F <- t(HB) %*% solve(H %*% V %*% t(H)) %*% HB/(q * tauhat^2)
    T <- q * F
    list(Fstat = F, Fpval = 1 - pf(F, q, min.n - p), Tstat = T, 
        Tpval = 1 - pchisq(T, q))
}

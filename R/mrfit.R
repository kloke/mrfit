mrfit <-
function (x, y, center, beta0 = suppressWarnings(jaeckel(x, y)$par)) 
{
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    a <- unique(center)
    nc <- length(a)
    for (i in 1:nc) {
        x1 <- as.matrix(x[center == a[i], ])
        n1 <- nrow(x1)
        x[center == a[i], ] <- x1 - matrix(rep(1, n1), ncol = 1) %*% 
            apply(x1, 2, mean)
    }
    fit <- optim(beta0, mrdisp, method = "BFGS", x = x, y = y, 
        center = center, gr = mrgrad)
    betahat <- fit$par
    xpxi <- chol2inv(chol(crossprod(x)))
    tauhat <- mrwilcoxontau(x, y, center, betahat)
    sebetahat <- tauhat * sqrt(diag(xpxi))
    res <- list(betahat = betahat, tauhat = tauhat, disp = mrdisp(x,y, center, betahat), 
		sebetahat = sebetahat,x=x,y=y,center=center)
    class(res) <- "mrfit"
    res
}

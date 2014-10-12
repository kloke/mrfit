rstudent.mrfit <-
function (model,...) 
{
	fit<-model ; rm(model)
    x <- as.matrix(fit$x)
	y<-fit$y
	center<-fit$center
    N <- nrow(x)
    p <- ncol(x)
    xs <- matrix(nrow = N, ncol = p)
    e <- y - x %*% fit$betahat
    r <- e - median(e)
    uc <- unique(center)
    nc <- length(uc)
    nj <- rep(0, nc)
    for (j in 1:nc) {
        r1 <- r[center == uc[j]]
        nj[j] <- length(r1)
        x1 <- as.matrix(x[center == uc[j], ])
        xs[center == uc[j], ] <- x1 - matrix(rep(1, nj[j]), ncol = 1) %*% 
            apply(x1, 2, mean)
    }
    varcomp <- varcomp.mrfit(fit)
    sigmae <- varcomp$sigmae
    sigmab <- varcomp$sigmab
    sigma2 <- sigmae^2 + sigmab^2
    rho <- sigmab^2/sigma2
    Se <- matrix(0, ncol = N, nrow = N)
    for (j in 1:nc) {
        Se[center == uc[j], center == uc[j]] <- (1 - rho) * diag(rep(1, 
            nj[j])) + rho * outer(rep(1, nj[j]), rep(1, nj[j]))
    }
    Se <- sigma2 * Se
    V <- solve(t(xs) %*% xs)
    A2 <- fit$tauhat^2 * x %*% V %*% t(x)
    A3 <- fit$disp/N * fit$tauhat * xs %*% V %*% t(x)
    Cmr <- Se + A2 - A3 - t(A3)
    return(e/sqrt(diag(Cmr)))
}

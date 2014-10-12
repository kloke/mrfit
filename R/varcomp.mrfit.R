varcomp.mrfit <-
function (fit)
{
	y<-fit$y
	center<-fit$center
    x <- as.matrix(fit$x)
    e <- y - x %*% fit$betahat
    r <- e - median(e)
    uc <- unique(center)
    nc <- length(uc)
    nj <- rep(0, nc)
    b <- rep(0, nc)
    for (j in 1:nc) {
        r1 <- r[center == uc[j]]
        nj[j] <- length(r1)
        b[j] <- median(r1)
    }
    ehat <- e - model.matrix(~as.factor(center)) %*% b
    list(sigmab = mad(b), sigmae = mad(ehat))
}

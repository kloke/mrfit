mrwilcoxontau <-
function (x, y, center, beta) 
{
    x <- as.matrix(x)
    a <- unique(center)
    nc <- length(a)
    p <- ncol(x)
    tauhats <- rep(0, nc)
	minn<-min(tapply(!is.na(y),center,sum))
    for (i in 1:nc) {
        x1 <- as.matrix(x[center == a[i], ])
        y1 <- y[center == a[i]]
        tauhats[i] <- rashidtau(y1 - x1 %*% beta, p,hcorrect=minn>p+1)
    }
    mean(tauhats)
}

\name{mrgrad}
\alias{mrgrad}
\title{
Gradiant of the MR dispersion function.
}
\description{
Gradiant, with respect to beta, of the MR dispersion function.
Used in minimization routine.
}
\usage{
mrgrad(x, y, center, beta)
}
\arguments{
  \item{x}{n by p design matrix}
  \item{y}{n by 1 response vector}
  \item{center}{n by 1 vector denoting block (cluster) membership}
  \item{beta}{ p by 1 vector }
}
\value{Gradient of MR dispersion function evaluated at beta.}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{
John Kloke \email{kloke@biostat.wisc.edu}
}

\examples{
### Internal function ###

## The function is currently defined as
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
        e <- y1 - x1 \%*\% beta
        sj <- as.matrix(sqrt(12) * (rank(e, ties.method = "random")/(nj + 
            1) - 0.5))
        Smat[, j] <- t(x1) \%*\% sj
    }
    apply(Smat, 1, sum)
  }
}

\name{mrdisp}
\alias{mrdisp}
\title{
Multiple Rankings (MR) dispersion function.
}
\description{
A seperate ranking (and dispersion function) is formed for each of the centers/blocks/clusters.  
The MR dispersion function is the sum of these separate dispersion functions.
The objective function for the MR estimator is the minimizor of this MR dispersion function.
}
\usage{
mrdisp(x, y, center, beta)
}
\arguments{
  \item{x}{n by p design matrix}
  \item{y}{n by 1 response vector}
  \item{center}{n by 1 vector denoting block (cluster) membership}
  \item{beta}{ p by 1 vector }
}
\details{
The multiple rankings (MR) dispersion function is defined as
D(beta) = sum Dj(beta) where Dj(beta) is Jaeckel's (1972) dispersion function for the jth cluster/center/block.
A separate set of rankings is computed for each center.
D(beta) is the objective function to minimize for the MR estimate of beta.
}
\value{
Returns the MR dispersion function evaluated at beta.
}
\references{
Jaeckel, L. A. (1972). Estimating regression coefficients by minimizing the dispersion of residuals. \emph{Annals of Mathematical Statistics}, 43, 1449 - 1458.

Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}}

\seealso{
\code{\link{mrfit}}
}
\examples{
### Internal function ###
## The function is currently defined as
function (x, y, center, beta) 
{
    a <- unique(center)
    nc <- length(a)
    Dvec <- rep(0, nc)
    for (j in 1:nc) {
        x1 <- as.matrix(x[center == a[j], ])
        y1 <- y[center == a[j]]
        nj <- length(y1)
        e <- y1 - x1 \%*\% beta
        sj <- as.matrix(sqrt(12) * (rank(e, ties.method = "random")/(nj + 
            1) - 0.5))
        Dvec[j] <- t(sj) \%*\% e
    }
    sum(Dvec)
  }
}

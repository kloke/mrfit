\name{mrfit-package}
\alias{mrfit-package}
\alias{mrfit}
\docType{package}
\title{
Robust rank-based estimates for cluster correlated data.
}
\description{
Multiple rankings (MR) estimates for cluster correlated data.
}
\details{
\tabular{ll}{
Package: \tab mrfit\cr
Type: \tab Package\cr
Version: \tab 0.00\cr
Date: \tab 2012-10-17\cr
License: \tab GPL\cr
}
}
\author{
John Kloke
Maintainer: John Kloke <kloke@biostat.wisc.edu>
}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\keyword{ package }
\seealso{
\code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
data(gly4gen)
X<-with(gly4gen,model.matrix(~as.factor(group)-1))
X<-X[,2:5]
y<-gly4gen$diffgly4
block<-gly4gen$center
mrfit(X,y,block,rfit(y~X)$coef[2:5])
}

\name{mrfit}
\alias{mrfit}
\title{
Multiple rankings (MR) estimates for cluster correlated data..
}
\description{
Returns the MR fit (and results for inference) for a linear model with cluster correlated data.
The objective function is a sum of dispersion functions, one for each cluster/center/block.
}
\usage{
mrfit(x, y, center, beta0 = jaeckel(x, y)$par)
}
\arguments{
  \item{x}{ n by p design matrix }
  \item{y}{ n by 1 response vector }
  \item{center}{ n by 1 vector denoting cluster membership }
  \item{beta0}{ initial fit (default is JR estimate)  }
}
\details{
The multiple rankings (MR) dispersion function is defined as
D(beta) = sum Dj(beta) where Dj(beta) is Jaeckel's (1972) dispersion function for the jth cluster/center/block.
A separate set of rankings is computed for each center.
D(beta) is the objective function to minimize for the MR estimate of beta.
}
\value{
	Returns an object of class mrfit
  \item{betahat}{Estimated regression coefficients}
  \item{tauhat}{Estimate of the scale parameter tau}
  \item{disp}{Value of the MR dispersion function evaluated at betahat}
  \item{sebetahat}{estimated standard errors of the regression coefficients} 
  \item{x}{Centered design matrix}
  \item{y}{response vector}
  \item{center}{vector denoting cluster membership}
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}}

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# a simple simulated example w/ 4 equal sized blocks #
x<-matrix(rnorm(40),ncol=2); y<-rnorm(20) ; g<-rep(1:4,each=5)
fit<-mrfit(x,y,g)
}
\keyword{robust}
\keyword{nonparametric }

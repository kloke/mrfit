\name{rstudent.mrfit}
\alias{rstudent.mrfit}
\title{ Studentized residuals based on an MR fit.  }
\description{ 
Studentized residuals based on an MR fit.  
Assumes a compound symmetric variance covariance structure.
}
\usage{
\method{rstudent}{mrfit}(model, ...)
}
\arguments{
  \item{model}{model fit from mrfit }
  \item{\dots}{ additional arguments.  currently not used.  }
}
\details{
Studentized residuals are based on a first order approximation.
The residuals are standarized so that the approximate asymptotic variance is one.
}
\value{
A vector of Studentized residuals.
}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.

Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{John Kloke \email{kloke@biostat.wisc.edu}}

\seealso{
\code{\link{mrfit}}
}
\examples{
# a simple simulated example w/ 4 equal sized blocks #
x<-matrix(rnorm(40),ncol=2); y<-rnorm(20) ; g<-rep(1:4,each=5)
qqnorm(rstudent(mrfit(x,y,g)))
}

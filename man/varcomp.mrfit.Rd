\name{varcomp.mrfit}
\alias{varcomp.mrfit}
\title{Estimates of variance components for compound symmetric model based on mrfit}
\description{
Returns estimates of variance components for the within and between center variability based on a MR fit.
}
\usage{
varcomp.mrfit(fit)
}
\arguments{
  \item{fit}{fit from mrfit}
}
%\details{

%}
%\value{
%}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{ John Kloke \email{kloke@biostat.wisc.edu} }
\examples{
m<-40; n<-4; N<-m*n ; p<-2
block<-rep(1:m,each=n)
Z<-model.matrix(~as.factor(block))
b<-rnorm(m,sd=3)
e<-rnorm(N,sd=1)
y<-Z%*%b+e
x<-matrix(rnorm(p*N),ncol=p)
#### for some reason the following code does not run with R CMD check
#### but runs otherwise ##
# fit<-mrfit(x,y,block,beta0=rep(0,2))
# vc<-varcomp.mrfit(fit)
### interclass correlation coefficient should be close to 9/10 ###
# with(vc, sigmab^2/(sigmab^2+sigmae^2))
}

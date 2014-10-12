\name{rashidtau}
\alias{rashidtau}
\title{
Estimate of tau for MR.
}
\description{
Estimate of tau for MR.
}
\usage{
rashidtau(resd, p, delta = 0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resd}{
%%     ~~Describe \code{resd} here~~
}
  \item{p}{
%%     ~~Describe \code{p} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Rashid, M.M., McKean, J.W., Kloke, J.D. (2011).  R Estimates and Associated Inferences for Mixed Models with Covariates in a Multi-Center Clinical Trial.  \emph{Statistics in Biopharmaceutical Research}.
}
\author{
John Kloke \email{kloke@biostat.wisc.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (resd, p, delta = 0.8) 
{
    n = length(resd)
    rd = sort(matrix(abs(outer(resd, resd, "-")), ncol = 1))
    tn = quantile(rd, delta)/sqrt(n)
    gtn = 0
    for (i in 1:n^2) {
        if (rd[i] <= tn) {
            gtn = gtn + 1
        }
        else {
            break
        }
    }
    gtn = gtn/n^2
    rashidtau = 1/((sqrt(3)/tn) * gtn * sqrt((n - p - 1)/n))
    rashidtau
  }
}
